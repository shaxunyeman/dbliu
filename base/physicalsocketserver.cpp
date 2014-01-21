/*
* libjingle
* Copyright 2004--2005, Google Inc.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*
*  1. Redistributions of source code must retain the above copyright notice,
*     this list of conditions and the following disclaimer.
*  2. Redistributions in binary form must reproduce the above copyright notice,
*     this list of conditions and the following disclaimer in the documentation
*     and/or other materials provided with the distribution.
*  3. The name of the author may not be used to endorse or promote products
*     derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
* WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
* MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
* EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
* PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
* OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
* WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
* OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
* ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#if defined(_MSC_VER) && _MSC_VER < 1300
#pragma warning(disable:4786)
#endif

#include <cassert>

#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#undef SetPort
#endif

#include <algorithm>
#include <map>

#include "base/basictypes.h"
#include "base/byteorder.h"
#include "base/common.h"
#include "base/logging.h"
#include "base/nethelpers.h"
#include "base/physicalsocketserver.h"
#include "base/timeutils.h"
//#include "base/winping.h"
#include "base/win32socketinit.h"

// stm: this will tell us if we are on OSX
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef WIN32
typedef char* SockOptArg;
#endif

namespace base {

	// Standard MTUs, from RFC 1191
	const uint16 PACKET_MAXIMUMS[] = {
		65535,    // Theoretical maximum, Hyperchannel
		32000,    // Nothing
		17914,    // 16Mb IBM Token Ring
		8166,     // IEEE 802.4
		//4464,   // IEEE 802.5 (4Mb max)
		4352,     // FDDI
		//2048,   // Wideband Network
		2002,     // IEEE 802.5 (4Mb recommended)
		//1536,   // Expermental Ethernet Networks
		//1500,   // Ethernet, Point-to-Point (default)
		1492,     // IEEE 802.3
		1006,     // SLIP, ARPANET
		//576,    // X.25 Networks
		//544,    // DEC IP Portal
		//512,    // NETBIOS
		508,      // IEEE 802/Source-Rt Bridge, ARCNET
		296,      // Point-to-Point (low delay)
		68,       // Official minimum
		0,        // End of list marker
	};

	static const int IP_HEADER_SIZE = 20u;
	static const int IPV6_HEADER_SIZE = 40u;
	static const int ICMP_HEADER_SIZE = 8u;
	static const int ICMP_PING_TIMEOUT_MILLIS = 10000u;

	class PhysicalSocket : public AsyncSocket, public sigslot::has_slots<> {
	public:
		PhysicalSocket(PhysicalSocketServer* ss, SOCKET s = INVALID_SOCKET)
			: ss_(ss), s_(s), enabled_events_(0), error_(0),
			state_((s == INVALID_SOCKET) ? CS_CLOSED : CS_CONNECTED),
			resolver_(NULL) {
#ifdef WIN32
				// EnsureWinsockInit() ensures that winsock is initialized. The default
				// version of this function doesn't do anything because winsock is
				// initialized by constructor of a static object. If neccessary libjingle
				// users can link it with a different version of this function by replacing
				// win32socketinit.cc. See win32socketinit.cc for more details.
				EnsureWinsockInit();
#endif
				if (s_ != INVALID_SOCKET) {
					enabled_events_ = DE_READ | DE_WRITE;

					int type = SOCK_STREAM;
					socklen_t len = sizeof(type);
					VERIFY(0 == getsockopt(s_, SOL_SOCKET, SO_TYPE, (SockOptArg)&type, &len));
					udp_ = (SOCK_DGRAM == type);
				}
		}

		virtual ~PhysicalSocket() {
			Close();
		}

		// Creates the underlying OS socket (same as the "socket" function).
		virtual bool Create(int family, int type) {
			Close();
			s_ = ::socket(family, type, 0);
			udp_ = (SOCK_DGRAM == type);
			UpdateLastError();
			if (udp_)
				enabled_events_ = DE_READ | DE_WRITE;
			return s_ != INVALID_SOCKET;
		}

		SocketAddress GetLocalAddress() const {
			sockaddr_storage addr_storage = {0};
			socklen_t addrlen = sizeof(addr_storage);
			sockaddr* addr = reinterpret_cast<sockaddr*>(&addr_storage);
			int result = ::getsockname(s_, addr, &addrlen);
			SocketAddress address;
			if (result >= 0) {
				SocketAddressFromSockAddrStorage(addr_storage, &address);
			} else {
				LOG(LS_WARNING) << "GetLocalAddress: unable to get local addr, socket="
					<< s_;
			}
			return address;
		}

		SocketAddress GetRemoteAddress() const {
			sockaddr_storage addr_storage = {0};
			socklen_t addrlen = sizeof(addr_storage);
			sockaddr* addr = reinterpret_cast<sockaddr*>(&addr_storage);
			int result = ::getpeername(s_, addr, &addrlen);
			SocketAddress address;
			if (result >= 0) {
				SocketAddressFromSockAddrStorage(addr_storage, &address);
			} else {
				LOG(LS_WARNING) << "GetRemoteAddress: unable to get remote addr, socket="
					<< s_;
			}
			return address;
		}

		int Bind(const SocketAddress& bind_addr) {
			sockaddr_storage addr_storage;
			size_t len = bind_addr.ToSockAddrStorage(&addr_storage);
			sockaddr* addr = reinterpret_cast<sockaddr*>(&addr_storage);
			int err = ::bind(s_, addr, static_cast<int>(len));
			UpdateLastError();
#ifdef _DEBUG
			if (0 == err) {
				dbg_addr_ = "Bound @ ";
				dbg_addr_.append(GetLocalAddress().ToString());
			}
#endif  // _DEBUG
			return err;
		}

		int Connect(const SocketAddress& addr) {
			// TODO: Implicit creation is required to reconnect...
			// ...but should we make it more explicit?
			if (state_ != CS_CLOSED) {
				SetError(EALREADY);
				return SOCKET_ERROR;
			}
			if (addr.IsUnresolved()) {
				LOG(LS_VERBOSE) << "Resolving addr in PhysicalSocket::Connect";
				resolver_ = new AsyncResolver();
				resolver_->set_address(addr);
				resolver_->SignalWorkDone.connect(this, &PhysicalSocket::OnResolveResult);
				resolver_->Start();
				state_ = CS_CONNECTING;
				return 0;
			}

			return DoConnect(addr);
		}

		int DoConnect(const SocketAddress& connect_addr) {
			if ((s_ == INVALID_SOCKET) &&
				!Create(connect_addr.family(), SOCK_STREAM)) {
					return SOCKET_ERROR;
			}
			sockaddr_storage addr_storage;
			size_t len = connect_addr.ToSockAddrStorage(&addr_storage);
			sockaddr* addr = reinterpret_cast<sockaddr*>(&addr_storage);
			int err = ::connect(s_, addr, static_cast<int>(len));
			UpdateLastError();
			if (err == 0) {
				state_ = CS_CONNECTED;
			} else if (IsBlockingError(error_)) {
				state_ = CS_CONNECTING;
				enabled_events_ |= DE_CONNECT;
			} else {
				return SOCKET_ERROR;
			}

			enabled_events_ |= DE_READ | DE_WRITE;
			return 0;
		}

		int GetError() const {
			return error_;
		}

		void SetError(int error) {
			error_ = error;
		}

		ConnState GetState() const {
			return state_;
		}

		int GetOption(Option opt, int* value) {
			int slevel;
			int sopt;
			if (TranslateOption(opt, &slevel, &sopt) == -1)
				return -1;
			socklen_t optlen = sizeof(*value);
			int ret = ::getsockopt(s_, slevel, sopt, (SockOptArg)value, &optlen);
			if (ret != -1 && opt == OPT_DONTFRAGMENT) {
#ifdef LINUX
				*value = (*value != IP_PMTUDISC_DONT) ? 1 : 0;
#endif
			}
			return ret;
		}

		int SetOption(Option opt, int value) {
			int slevel;
			int sopt;
			if (TranslateOption(opt, &slevel, &sopt) == -1)
				return -1;
			if (opt == OPT_DONTFRAGMENT) {
#ifdef LINUX
				value = (value) ? IP_PMTUDISC_DO : IP_PMTUDISC_DONT;
#endif
			}
			return ::setsockopt(s_, slevel, sopt, (SockOptArg)&value, sizeof(value));
		}

		int Send(const void *pv, size_t cb) {
			int sent = ::send(s_, reinterpret_cast<const char *>(pv), (int)cb,
#ifdef LINUX
				// Suppress SIGPIPE. Without this, attempting to send on a socket whose
				// other end is closed will result in a SIGPIPE signal being raised to
				// our process, which by default will terminate the process, which we
				// don't want. By specifying this flag, we'll just get the error EPIPE
				// instead and can handle the error gracefully.
				MSG_NOSIGNAL
#else
				0
#endif
				);
			UpdateLastError();
			// We have seen minidumps where this may be false.
			ASSERT(sent <= static_cast<int>(cb));
			if ((sent < 0) && IsBlockingError(error_)) {
				enabled_events_ |= DE_WRITE;
			}
			return sent;
		}

		int SendTo(const void* buffer, size_t length, const SocketAddress& addr) {
			sockaddr_storage saddr;
			size_t len = addr.ToSockAddrStorage(&saddr);
			int sent = ::sendto(
				s_, static_cast<const char *>(buffer), static_cast<int>(length),
#ifdef LINUX
				// Suppress SIGPIPE. See above for explanation.
				MSG_NOSIGNAL,
#else
				0,
#endif
				reinterpret_cast<sockaddr*>(&saddr), static_cast<int>(len));
			UpdateLastError();
			// We have seen minidumps where this may be false.
			ASSERT(sent <= static_cast<int>(length));
			if ((sent < 0) && IsBlockingError(error_)) {
				enabled_events_ |= DE_WRITE;
			}
			return sent;
		}

		int Recv(void* buffer, size_t length) {
			int received = ::recv(s_, static_cast<char*>(buffer),
				static_cast<int>(length), 0);
			if ((received == 0) && (length != 0)) {
				// Note: on graceful shutdown, recv can return 0.  In this case, we
				// pretend it is blocking, and then signal close, so that simplifying
				// assumptions can be made about Recv.
				LOG(LS_WARNING) << "EOF from socket; deferring close event";
				// Must turn this back on so that the select() loop will notice the close
				// event.
				enabled_events_ |= DE_READ;
				error_ = EWOULDBLOCK;
				return SOCKET_ERROR;
			}
			UpdateLastError();
			bool success = (received >= 0) || IsBlockingError(error_);
			if (udp_ || success) {
				enabled_events_ |= DE_READ;
			}
			if (!success) {
				LOG(LS_VERBOSE) << "Error = " << error_;
			}
			return received;
		}

		int RecvFrom(void* buffer, size_t length, SocketAddress *out_addr) {
			sockaddr_storage addr_storage;
			socklen_t addr_len = sizeof(addr_storage);
			sockaddr* addr = reinterpret_cast<sockaddr*>(&addr_storage);
			int received = ::recvfrom(s_, static_cast<char*>(buffer),
				static_cast<int>(length), 0, addr, &addr_len);
			UpdateLastError();
			if ((received >= 0) && (out_addr != NULL))
				SocketAddressFromSockAddrStorage(addr_storage, out_addr);
			bool success = (received >= 0) || IsBlockingError(error_);
			if (udp_ || success) {
				enabled_events_ |= DE_READ;
			}
			if (!success) {
				LOG(LS_VERBOSE) << "Error = " << error_;
			}
			return received;
		}

		int Listen(int backlog) {
			int err = ::listen(s_, backlog);
			UpdateLastError();
			if (err == 0) {
				state_ = CS_CONNECTING;
				enabled_events_ |= DE_ACCEPT;
#ifdef _DEBUG
				dbg_addr_ = "Listening @ ";
				dbg_addr_.append(GetLocalAddress().ToString());
#endif  // _DEBUG
			}
			return err;
		}

		AsyncSocket* Accept(SocketAddress *out_addr) {
			sockaddr_storage addr_storage;
			socklen_t addr_len = sizeof(addr_storage);
			sockaddr* addr = reinterpret_cast<sockaddr*>(&addr_storage);
			SOCKET s = ::accept(s_, addr, &addr_len);
			UpdateLastError();
			if (s == INVALID_SOCKET)
				return NULL;
			enabled_events_ |= DE_ACCEPT;
			if (out_addr != NULL)
				SocketAddressFromSockAddrStorage(addr_storage, out_addr);
			return ss_->WrapSocket(s);
		}

		int Close() {
			if (s_ == INVALID_SOCKET)
				return 0;
			int err = ::closesocket(s_);
			UpdateLastError();
			s_ = INVALID_SOCKET;
			state_ = CS_CLOSED;
			enabled_events_ = 0;
			if (resolver_) {
				resolver_->Destroy(false);
				resolver_ = NULL;
			}
			return err;
		}

		int EstimateMTU(uint16* mtu) {
			return 0;
		}

		SocketServer* socketserver() { return ss_; }

	protected:
		void OnResolveResult(SignalThread* thread) {
			if (thread != resolver_) {
				return;
			}

			int error = resolver_->error();
			if (error == 0) {
				error = DoConnect(resolver_->address());
			} else {
				Close();
			}

			if (error) {
				error_ = error;
				SignalCloseEvent(this, error_);
			}
		}

		void UpdateLastError() {
			error_ = LAST_SYSTEM_ERROR;
		}

		static int TranslateOption(Option opt, int* slevel, int* sopt) {
			switch (opt) {
			case OPT_DONTFRAGMENT:
#ifdef WIN32
				*slevel = IPPROTO_IP;
				*sopt = IP_DONTFRAGMENT;
				break;
#elif defined(IOS) || defined(OSX) || defined(BSD)
				LOG(LS_WARNING) << "Socket::OPT_DONTFRAGMENT not supported.";
				return -1;
#elif defined(POSIX)
				*slevel = IPPROTO_IP;
				*sopt = IP_MTU_DISCOVER;
				break;
#endif
			case OPT_RCVBUF:
				*slevel = SOL_SOCKET;
				*sopt = SO_RCVBUF;
				break;
			case OPT_SNDBUF:
				*slevel = SOL_SOCKET;
				*sopt = SO_SNDBUF;
				break;
			case OPT_NODELAY:
				*slevel = IPPROTO_TCP;
				*sopt = TCP_NODELAY;
				break;
			default:
				ASSERT(false);
				return -1;
			}
			return 0;
		}

		PhysicalSocketServer* ss_;
		SOCKET s_;
		uint8 enabled_events_;
		bool udp_;
		int error_;
		ConnState state_;
		AsyncResolver* resolver_;

#ifdef _DEBUG
		std::string dbg_addr_;
#endif  // _DEBUG;
	};

	static uint32 FlagsToEvents(uint32 events) {
		uint32 ffFD = FD_CLOSE;
		if (events & DE_READ)
			ffFD |= FD_READ;
		if (events & DE_WRITE)
			ffFD |= FD_WRITE;
		if (events & DE_CONNECT)
			ffFD |= FD_CONNECT;
		if (events & DE_ACCEPT)
			ffFD |= FD_ACCEPT;
		return ffFD;
	}

	class EventDispatcher : public Dispatcher {
	public:
		EventDispatcher(PhysicalSocketServer *ss) : ss_(ss) {
			hev_ = WSACreateEvent();
			if (hev_) {
				ss_->Add(this);
			}
		}

		~EventDispatcher() {
			if (hev_ != NULL) {
				ss_->Remove(this);
				WSACloseEvent(hev_);
				hev_ = NULL;
			}
		}

		virtual void Signal() {
			if (hev_ != NULL)
				WSASetEvent(hev_);
		}

		virtual uint32 GetRequestedEvents() {
			return 0;
		}

		virtual void OnPreEvent(uint32 ff) {
			WSAResetEvent(hev_);
		}

		virtual void OnEvent(uint32 ff, int err) {
		}

		virtual WSAEVENT GetWSAEvent() {
			return hev_;
		}

		virtual SOCKET GetSocket() {
			return INVALID_SOCKET;
		}

		virtual bool CheckSignalClose() { return false; }

	private:
		PhysicalSocketServer* ss_;
		WSAEVENT hev_;
	};

	class SocketDispatcher : public Dispatcher, public PhysicalSocket {
	public:
		static int next_id_;
		int id_;
		bool signal_close_;
		int signal_err_;

		SocketDispatcher(PhysicalSocketServer* ss)
			: PhysicalSocket(ss),
			id_(0),
			signal_close_(false) {
		}

		SocketDispatcher(SOCKET s, PhysicalSocketServer* ss)
			: PhysicalSocket(ss, s),
			id_(0),
			signal_close_(false) {
		}

		virtual ~SocketDispatcher() {
			Close();
		}

		bool Initialize() {
			ASSERT(s_ != INVALID_SOCKET);
			// Must be a non-blocking
			u_long argp = 1;
			ioctlsocket(s_, FIONBIO, &argp);
			ss_->Add(this);
			return true;
		}

		virtual bool Create(int type) {
			return Create(AF_INET, type);
		}

		virtual bool Create(int family, int type) {
			// Create socket
			if (!PhysicalSocket::Create(family, type))
				return false;

			if (!Initialize())
				return false;

			do { id_ = ++next_id_; } while (id_ == 0);
			return true;
		}

		virtual int Close() {
			if (s_ == INVALID_SOCKET)
				return 0;

			id_ = 0;
			signal_close_ = false;
			ss_->Remove(this);
			return PhysicalSocket::Close();
		}

		virtual uint32 GetRequestedEvents() {
			return enabled_events_;
		}

		virtual void OnPreEvent(uint32 ff) {
			if ((ff & DE_CONNECT) != 0)
				state_ = CS_CONNECTED;
			// We set CS_CLOSED from CheckSignalClose.
		}

		virtual void OnEvent(uint32 ff, int err) {
			int cache_id = id_;
			// Make sure we deliver connect/accept first. Otherwise, consumers may see
			// something like a READ followed by a CONNECT, which would be odd.
			if (((ff & DE_CONNECT) != 0) && (id_ == cache_id)) {
				if (ff != DE_CONNECT)
					LOG(LS_VERBOSE) << "Signalled with DE_CONNECT: " << ff;
				enabled_events_ &= ~DE_CONNECT;
#ifdef _DEBUG
				dbg_addr_ = "Connected @ ";
				dbg_addr_.append(GetRemoteAddress().ToString());
#endif  // _DEBUG
				SignalConnectEvent(this);
			}
			if (((ff & DE_ACCEPT) != 0) && (id_ == cache_id)) {
				enabled_events_ &= ~DE_ACCEPT;
				SignalReadEvent(this);
			}
			if ((ff & DE_READ) != 0) {
				enabled_events_ &= ~DE_READ;
				SignalReadEvent(this);
			}
			if (((ff & DE_WRITE) != 0) && (id_ == cache_id)) {
				enabled_events_ &= ~DE_WRITE;
				SignalWriteEvent(this);
			}
			if (((ff & DE_CLOSE) != 0) && (id_ == cache_id)) {
				signal_close_ = true;
				signal_err_ = err;
			}
		}

		virtual WSAEVENT GetWSAEvent() {
			return WSA_INVALID_EVENT;
		}

		virtual SOCKET GetSocket() {
			return s_;
		}

		virtual bool CheckSignalClose() {
			if (!signal_close_)
				return false;

			char ch;
			if (recv(s_, &ch, 1, MSG_PEEK) > 0)
				return false;

			state_ = CS_CLOSED;
			signal_close_ = false;
			SignalCloseEvent(this, signal_err_);
			return true;
		}
	};

	int SocketDispatcher::next_id_ = 0;

	// Sets the value of a boolean value to false when signaled.
	class Signaler : public EventDispatcher {
	public:
		Signaler(PhysicalSocketServer* ss, bool* pf)
			: EventDispatcher(ss), pf_(pf) {
		}
		virtual ~Signaler() { }

		void OnEvent(uint32 ff, int err) {
			if (pf_)
				*pf_ = false;
		}

	private:
		bool *pf_;
	};

	PhysicalSocketServer::PhysicalSocketServer()
		: fWait_(false),
		last_tick_tracked_(0),
		last_tick_dispatch_count_(0) {
			signal_wakeup_ = new Signaler(this, &fWait_);
			socket_ev_ = WSACreateEvent();
	}

	PhysicalSocketServer::~PhysicalSocketServer() {
		WSACloseEvent(socket_ev_);
		delete signal_wakeup_;
		ASSERT(dispatchers_.empty());
	}

	void PhysicalSocketServer::WakeUp() {
		signal_wakeup_->Signal();
	}

	Socket* PhysicalSocketServer::CreateSocket(int type) {
		return CreateSocket(AF_INET, type);
	}

	Socket* PhysicalSocketServer::CreateSocket(int family, int type) {
		PhysicalSocket* socket = new PhysicalSocket(this);
		if (socket->Create(family, type)) {
			return socket;
		} else {
			delete socket;
			return 0;
		}
	}

	AsyncSocket* PhysicalSocketServer::CreateAsyncSocket(int type) {
		return CreateAsyncSocket(AF_INET, type);
	}

	AsyncSocket* PhysicalSocketServer::CreateAsyncSocket(int family, int type) {
		SocketDispatcher* dispatcher = new SocketDispatcher(this);
		if (dispatcher->Create(family, type)) {
			return dispatcher;
		} else {
			delete dispatcher;
			return 0;
		}
	}

	AsyncSocket* PhysicalSocketServer::WrapSocket(SOCKET s) {
		SocketDispatcher* dispatcher = new SocketDispatcher(s, this);
		if (dispatcher->Initialize()) {
			return dispatcher;
		} else {
			delete dispatcher;
			return 0;
		}
	}

	void PhysicalSocketServer::Add(Dispatcher *pdispatcher) {
		CritScope cs(&crit_);
		// Prevent duplicates. This can cause dead dispatchers to stick around.
		DispatcherList::iterator pos = std::find(dispatchers_.begin(),
			dispatchers_.end(),
			pdispatcher);
		if (pos != dispatchers_.end())
			return;
		dispatchers_.push_back(pdispatcher);
	}

	void PhysicalSocketServer::Remove(Dispatcher *pdispatcher) {
		CritScope cs(&crit_);
		DispatcherList::iterator pos = std::find(dispatchers_.begin(),
			dispatchers_.end(),
			pdispatcher);
		ASSERT(pos != dispatchers_.end());
		size_t index = pos - dispatchers_.begin();
		dispatchers_.erase(pos);
		for (IteratorList::iterator it = iterators_.begin(); it != iterators_.end();
			++it) {
				if (index < **it) {
					--**it;
				}
		}
	}

	bool PhysicalSocketServer::Wait(int cmsWait, bool process_io) {
		int cmsTotal = cmsWait;
		int cmsElapsed = 0;
		uint32 msStart = Time();

#if LOGGING
		if (last_tick_dispatch_count_ == 0) {
			last_tick_tracked_ = msStart;
		}
#endif

		fWait_ = true;
		while (fWait_) {
			std::vector<WSAEVENT> events;
			std::vector<Dispatcher *> event_owners;

			events.push_back(socket_ev_);

			{
				CritScope cr(&crit_);
				size_t i = 0;
				iterators_.push_back(&i);
				// Don't track dispatchers_.size(), because we want to pick up any new
				// dispatchers that were added while processing the loop.
				while (i < dispatchers_.size()) {
					Dispatcher* disp = dispatchers_[i++];
					if (!process_io && (disp != signal_wakeup_))
						continue;
					SOCKET s = disp->GetSocket();
					if (disp->CheckSignalClose()) {
						// We just signalled close, don't poll this socket
					} else if (s != INVALID_SOCKET) {
						WSAEventSelect(s,
							events[0],
							FlagsToEvents(disp->GetRequestedEvents()));
					} else {
						events.push_back(disp->GetWSAEvent());
						event_owners.push_back(disp);
					}
				}
				ASSERT(iterators_.back() == &i);
				iterators_.pop_back();
			}

			// Which is shorter, the delay wait or the asked wait?

			int cmsNext;
			if (cmsWait == kForever) {
				cmsNext = cmsWait;
			} else {
				cmsNext = _max(0, cmsTotal - cmsElapsed);
			}

			// Wait for one of the events to signal
			DWORD dw = WSAWaitForMultipleEvents(static_cast<DWORD>(events.size()),
				&events[0],
				false,
				cmsNext,
				false);

#if 0  // LOGGING
			// we track this information purely for logging purposes.
			last_tick_dispatch_count_++;
			if (last_tick_dispatch_count_ >= 1000) {
				int32 elapsed = TimeSince(last_tick_tracked_);
				LOG(INFO) << "PhysicalSocketServer took " << elapsed
					<< "ms for 1000 events";

				// If we get more than 1000 events in a second, we are spinning badly
				// (normally it should take about 8-20 seconds).
				ASSERT(elapsed > 1000);

				last_tick_tracked_ = Time();
				last_tick_dispatch_count_ = 0;
			}
#endif

			if (dw == WSA_WAIT_FAILED) {
				// Failed?
				// TODO: need a better strategy than this!
				int error = WSAGetLastError();
				ASSERT(false);
				return false;
			} else if (dw == WSA_WAIT_TIMEOUT) {
				// Timeout?
				return true;
			} else {
				// Figure out which one it is and call it
				CritScope cr(&crit_);
				int index = dw - WSA_WAIT_EVENT_0;
				if (index > 0) {
					--index; // The first event is the socket event
					event_owners[index]->OnPreEvent(0);
					event_owners[index]->OnEvent(0, 0);
				} else if (process_io) {
					size_t i = 0, end = dispatchers_.size();
					iterators_.push_back(&i);
					iterators_.push_back(&end);  // Don't iterate over new dispatchers.
					while (i < end) {
						Dispatcher* disp = dispatchers_[i++];
						SOCKET s = disp->GetSocket();
						if (s == INVALID_SOCKET)
							continue;

						WSANETWORKEVENTS wsaEvents;
						int err = WSAEnumNetworkEvents(s, events[0], &wsaEvents);
						if (err == 0) {

#if LOGGING
							{
								if ((wsaEvents.lNetworkEvents & FD_READ) &&
									wsaEvents.iErrorCode[FD_READ_BIT] != 0) {
										LOG(WARNING) << "PhysicalSocketServer got FD_READ_BIT error "
											<< wsaEvents.iErrorCode[FD_READ_BIT];
								}
								if ((wsaEvents.lNetworkEvents & FD_WRITE) &&
									wsaEvents.iErrorCode[FD_WRITE_BIT] != 0) {
										LOG(WARNING) << "PhysicalSocketServer got FD_WRITE_BIT error "
											<< wsaEvents.iErrorCode[FD_WRITE_BIT];
								}
								if ((wsaEvents.lNetworkEvents & FD_CONNECT) &&
									wsaEvents.iErrorCode[FD_CONNECT_BIT] != 0) {
										LOG(WARNING) << "PhysicalSocketServer got FD_CONNECT_BIT error "
											<< wsaEvents.iErrorCode[FD_CONNECT_BIT];
								}
								if ((wsaEvents.lNetworkEvents & FD_ACCEPT) &&
									wsaEvents.iErrorCode[FD_ACCEPT_BIT] != 0) {
										LOG(WARNING) << "PhysicalSocketServer got FD_ACCEPT_BIT error "
											<< wsaEvents.iErrorCode[FD_ACCEPT_BIT];
								}
								if ((wsaEvents.lNetworkEvents & FD_CLOSE) &&
									wsaEvents.iErrorCode[FD_CLOSE_BIT] != 0) {
										LOG(WARNING) << "PhysicalSocketServer got FD_CLOSE_BIT error "
											<< wsaEvents.iErrorCode[FD_CLOSE_BIT];
								}
							}
#endif
							uint32 ff = 0;
							int errcode = 0;
							if (wsaEvents.lNetworkEvents & FD_READ)
								ff |= DE_READ;
							if (wsaEvents.lNetworkEvents & FD_WRITE)
								ff |= DE_WRITE;
							if (wsaEvents.lNetworkEvents & FD_CONNECT) {
								if (wsaEvents.iErrorCode[FD_CONNECT_BIT] == 0) {
									ff |= DE_CONNECT;
								} else {
									ff |= DE_CLOSE;
									errcode = wsaEvents.iErrorCode[FD_CONNECT_BIT];
								}
							}
							if (wsaEvents.lNetworkEvents & FD_ACCEPT)
								ff |= DE_ACCEPT;
							if (wsaEvents.lNetworkEvents & FD_CLOSE) {
								ff |= DE_CLOSE;
								errcode = wsaEvents.iErrorCode[FD_CLOSE_BIT];
							}
							if (ff != 0) {
								disp->OnPreEvent(ff);
								disp->OnEvent(ff, errcode);
							}
						}
					}
					ASSERT(iterators_.back() == &end);
					iterators_.pop_back();
					ASSERT(iterators_.back() == &i);
					iterators_.pop_back();
				}

				// Reset the network event until new activity occurs
				WSAResetEvent(socket_ev_);
			}

			// Break?
			if (!fWait_)
				break;
			cmsElapsed = TimeSince(msStart);
			if ((cmsWait != kForever) && (cmsElapsed >= cmsWait)) {
				break;
			}
		}

		// Done
		return true;
	}

}  // namespace talk_base
