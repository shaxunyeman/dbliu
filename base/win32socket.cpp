#include "base/win32socket.h"
#include "base/win32window.h"
#include "base/logging.h"
#include "base/common.h"

namespace base {

	///////////////////////////////////////////////////////////////////////////////
	// Win32Socket
	///////////////////////////////////////////////////////////////////////////////

	// TODO: Move this to a common place where PhysicalSocketServer can
	// share it.
	// Standard MTUs
	static const uint16 PACKET_MAXIMUMS[] = {
		65535,    // Theoretical maximum, Hyperchannel
		32000,    // Nothing
		17914,    // 16Mb IBM Token Ring
		8166,     // IEEE 802.4
		// 4464   // IEEE 802.5 (4Mb max)
		4352,     // FDDI
		// 2048,  // Wideband Network
		2002,     // IEEE 802.5 (4Mb recommended)
		// 1536,  // Expermental Ethernet Networks
		// 1500,  // Ethernet, Point-to-Point (default)
		1492,     // IEEE 802.3
		1006,     // SLIP, ARPANET
		// 576,   // X.25 Networks
		// 544,   // DEC IP Portal
		// 512,   // NETBIOS
		508,      // IEEE 802/Source-Rt Bridge, ARCNET
		296,      // Point-to-Point (low delay)
		68,       // Official minimum
		0,        // End of list marker
	};

	static const int IP_HEADER_SIZE = 20u;
	static const int ICMP_HEADER_SIZE = 8u;
	static const int ICMP_PING_TIMEOUT_MILLIS = 10000u;

	// TODO: Enable for production builds also? Use FormatMessage?
#ifdef _DEBUG
	LPCSTR WSAErrorToString(int error, LPCSTR *description_result) {
		LPCSTR string = "Unspecified";
		LPCSTR description = "Unspecified description";
		switch (error) {
		case ERROR_SUCCESS:
			string = "SUCCESS";
			description = "Operation succeeded";
			break;
		case WSAEWOULDBLOCK:
			string = "WSAEWOULDBLOCK";
			description = "Using a non-blocking socket, will notify later";
			break;
		case WSAEACCES:
			string = "WSAEACCES";
			description = "Access denied, or sharing violation";
			break;
		case WSAEADDRNOTAVAIL:
			string = "WSAEADDRNOTAVAIL";
			description = "Address is not valid in this context";
			break;
		case WSAENETDOWN:
			string = "WSAENETDOWN";
			description = "Network is down";
			break;
		case WSAENETUNREACH:
			string = "WSAENETUNREACH";
			description = "Network is up, but unreachable";
			break;
		case WSAENETRESET:
			string = "WSANETRESET";
			description = "Connection has been reset due to keep-alive activity";
			break;
		case WSAECONNABORTED:
			string = "WSAECONNABORTED";
			description = "Aborted by host";
			break;
		case WSAECONNRESET:
			string = "WSAECONNRESET";
			description = "Connection reset by host";
			break;
		case WSAETIMEDOUT:
			string = "WSAETIMEDOUT";
			description = "Timed out, host failed to respond";
			break;
		case WSAECONNREFUSED:
			string = "WSAECONNREFUSED";
			description = "Host actively refused connection";
			break;
		case WSAEHOSTDOWN:
			string = "WSAEHOSTDOWN";
			description = "Host is down";
			break;
		case WSAEHOSTUNREACH:
			string = "WSAEHOSTUNREACH";
			description = "Host is unreachable";
			break;
		case WSAHOST_NOT_FOUND:
			string = "WSAHOST_NOT_FOUND";
			description = "No such host is known";
			break;
		}
		if (description_result) {
			*description_result = description;
		}
		return string;
	}

	void ReportWSAError(LPCSTR context, int error, const SocketAddress& address) {
		LPCSTR description_string;
		LPCSTR error_string = WSAErrorToString(error, &description_string);
		LOG(LS_INFO) << context << " = " << error
			<< " (" << error_string << ":" << description_string << ") ["
			<< address.ToString() << "]";
	}
#else
	void ReportWSAError(LPCSTR context, int error, const SocketAddress& address) {}
#endif
	/////////////////////////////////////////////////////////////////////////
	//Win32Socket::EventSink
	/////////////////////////////////////////////////////////////////////////

	#define WM_SOCKETNOTIFY  (WM_USER + 50)
	#define WM_DNSNOTIFY     (WM_USER + 51)

	struct Win32Socket::DnsLookup {
		HANDLE handle;
		uint16 port;
		char buffer[MAXGETHOSTSTRUCT];
	};

	class Win32Socket::EventSink : public Win32Window {
	public:
		explicit EventSink(Win32Socket * parent) : parent_(parent) { }

		void Dispose();

		virtual bool OnMessage(UINT uMsg, WPARAM wParam, LPARAM lParam,
			LRESULT& result);
		virtual void OnNcDestroy();

	private:
		bool OnSocketNotify(UINT uMsg, WPARAM wParam, LPARAM lParam, LRESULT& result);
		bool OnDnsNotify(WPARAM wParam, LPARAM lParam, LRESULT& result);

		Win32Socket * parent_;
	};

	void Win32Socket::EventSink::Dispose() {
		parent_ = NULL;
		if (::IsWindow(handle())) {
			::DestroyWindow(handle());
		} else {
			delete this;
		}
	}

	bool Win32Socket::EventSink::OnMessage(UINT uMsg, WPARAM wParam,
		LPARAM lParam, LRESULT& result) {
			switch (uMsg) {
			case WM_SOCKETNOTIFY:
			case WM_TIMER:
				return OnSocketNotify(uMsg, wParam, lParam, result);
			case WM_DNSNOTIFY:
				return OnDnsNotify(wParam, lParam, result);
			}
			return false;
	}

	bool Win32Socket::EventSink::OnSocketNotify(UINT uMsg, WPARAM wParam,
		LPARAM lParam, LRESULT& result) {
			result = 0;

			int wsa_event = WSAGETSELECTEVENT(lParam);
			int wsa_error = WSAGETSELECTERROR(lParam);

			// Treat connect timeouts as close notifications
			if (uMsg == WM_TIMER) {
				wsa_event = FD_CLOSE;
				wsa_error = WSAETIMEDOUT;
			}

			if (parent_)
				parent_->OnSocketNotify(static_cast<SOCKET>(wParam), wsa_event, wsa_error);
			return true;
	}

	bool Win32Socket::EventSink::OnDnsNotify(WPARAM wParam, LPARAM lParam,
		LRESULT& result) {
			result = 0;

			int error = WSAGETASYNCERROR(lParam);
			if (parent_)
				parent_->OnDnsNotify(reinterpret_cast<HANDLE>(wParam), error);
			return true;
	}

	void Win32Socket::EventSink::OnNcDestroy() {
		if (parent_) {
			LOG(LS_ERROR) << "EventSink hwnd is being destroyed, but the event sink"
				" hasn't yet been disposed.";
		} else {
			delete this;
		}
	}

	/////////////////////////////////////////////////////////////////////////
	//Win32Socket
	/////////////////////////////////////////////////////////////////////////

	Win32Socket::Win32Socket()
	:socket_(INVALID_SOCKET)
	,error_(0)
	,state_(CS_CLOSED)
	,closing_(false)
	,close_error_(0)
	,sink_(NULL)
	,dns_(NULL) {

	}

	Win32Socket::~Win32Socket(){
		Close();
	}

	bool Win32Socket::CreateT(int family,int type){
		Close();
		int proto = (SOCK_DGRAM == type) ? IPPROTO_UDP : IPPROTO_TCP;
		socket_ = ::WSASocket(family,type,proto,NULL,NULL,0);
		if (socket_ == INVALID_SOCKET) {
			UpdateLastError();
			return false;
		}

		if ((SOCK_DGRAM == type) && !SetAsync(FD_READ | FD_WRITE)) 
			return false;

		return true;
	}

	int Win32Socket::Attach(SOCKET s) {
		ASSERT(socket_ == INVALID_SOCKET);
		if (socket_ != INVALID_SOCKET)
			return SOCKET_ERROR;

		ASSERT(s != INVALID_SOCKET);
		if (s == INVALID_SOCKET)
			return SOCKET_ERROR;

		socket_ = s;
		state_ = CS_CONNECTED;

		if (!SetAsync(FD_READ | FD_WRITE | FD_CLOSE))
			return SOCKET_ERROR;

		return 0;
	}

	void Win32Socket::SetTimeout(int ms) {
		if (sink_) {
			::SetTimer(sink_->handle(),1,ms,0);
		}
	}

	int Win32Socket::Bind(const SocketAddress& addr) {
		ASSERT(socket_ != INVALID_SOCKET);
		if (socket_ == INVALID_SOCKET)
			return SOCKET_ERROR;

		sockaddr_storage saddr;
		size_t len = addr.ToSockAddrStorage(&saddr);
		int err = ::bind(socket_,
			reinterpret_cast<sockaddr*>(&saddr),
			static_cast<int>(len));
		UpdateLastError();
		return err;
	}

	// AsyncSocket Interface
	int Win32Socket::Connect(const SocketAddress& addr){
		if (state_ != CS_CLOSED) {
			SetError(EALREADY);
			return SOCKET_ERROR;
		}

		if (!addr.IsUnresolvedIP()) {
			return DoConnect(addr);
		}

		LOG(LS_INFO) << "async dns lookup (" << addr.hostname() << ")";
		DnsLookup *dns = new DnsLookup;
		if (!sink_) {
			CreateSink();
		}

		// TODO: Replace with IPv6 compatible lookup.
		dns->handle = WSAAsyncGetHostByName(sink_->handle(), WM_DNSNOTIFY,
			addr.hostname().c_str(), dns->buffer,
			sizeof(dns->buffer));

		if (!dns->handle) {
			LOG(LS_ERROR) << "WSAAsyncGetHostByName error: " << WSAGetLastError();
			delete dns;
			UpdateLastError();
			Close();
			return SOCKET_ERROR;
		}

		dns->port = addr.port();
		dns_ = dns;
		state_ = CS_CONNECTING;
		return 0;
	}

	int Win32Socket::Send(const void *pv,size_t cb){
		int send = ::send(socket_,
						reinterpret_cast<const char*>(pv),
						static_cast<int>(cb),
						0);
		UpdateLastError();
		return send;
	}

	int Win32Socket::SendTo(const void *pv, size_t cb, const SocketAddress& addr) {
		sockaddr_storage saddr;
		size_t addr_len = addr.ToSockAddrStorage(&saddr);
		int sent = ::sendto(socket_, reinterpret_cast<const char*>(pv),
			static_cast<int>(cb), 0,
			reinterpret_cast<sockaddr*>(&saddr),
			static_cast<int>(addr_len));
		UpdateLastError();
		return sent;
	}

	int Win32Socket::Recv(void *pv,size_t cb){
		int received = ::recv(socket_,
							static_cast<char*>(pv),
							static_cast<int>(cb),
							0);
		UpdateLastError();
		return received;
	}

	int Win32Socket::RecvFrom(void *pv, size_t cb, SocketAddress *paddr) {
		sockaddr_storage saddr;
		socklen_t addr_len = sizeof(saddr);
		int received = ::recvfrom(socket_, static_cast<char*>(pv),
			static_cast<int>(cb), 0,
			reinterpret_cast<sockaddr*>(&saddr), &addr_len);
		UpdateLastError();
		if (received != SOCKET_ERROR)
			SocketAddressFromSockAddrStorage(saddr, paddr);
		if (closing_ && received <= static_cast<int>(cb))
			PostClosed();
		return received;
	}

	int Win32Socket::Listen(int backlog) {
		int err = ::listen(socket_, backlog);
		if (!SetAsync(FD_ACCEPT))
			return SOCKET_ERROR;

		UpdateLastError();
		if (err == 0)
			state_ = CS_CONNECTING;
		return err;
	}

	Win32Socket *Win32Socket::Accept(SocketAddress *out_addr) {
		sockaddr_storage saddr;
		socklen_t addr_len = sizeof(saddr);
		SOCKET s = ::accept(socket_, reinterpret_cast<sockaddr*>(&saddr), &addr_len);
		UpdateLastError();
		if (s == INVALID_SOCKET)
			return NULL;
		if (out_addr)
			SocketAddressFromSockAddrStorage(saddr, out_addr);
		Win32Socket* socket = new Win32Socket;
		if (0 == socket->Attach(s))
			return socket;
		delete socket;
		return NULL;
	}

	int Win32Socket::Close(){
		int err = 0;
		if (socket_ != INVALID_SOCKET) {
			err = ::closesocket(socket_);
			socket_ = INVALID_SOCKET;
			closing_ = false;
			close_error_ = 0;
			UpdateLastError();
		}
		if (dns_) {
			WSACancelAsyncRequest(dns_->handle);
			delete dns_;
			dns_ = NULL;
		}
		if (sink_) {
			sink_->Dispose();
			sink_ = NULL;
		}
		addr_.Clear();
		state_ = CS_CLOSED;
		return err;
	}

	SocketAddress Win32Socket::GetLocalAddress() const {
		sockaddr_storage addr = {0};
		socklen_t addrlen = sizeof(addr);
		int result = ::getsockname(socket_, reinterpret_cast<sockaddr*>(&addr),
			&addrlen);
		SocketAddress address;
		if (result >= 0) {
			SocketAddressFromSockAddrStorage(addr, &address);
		} else {
			LOG(LS_WARNING) << "GetLocalAddress: unable to get local addr, socket="
				<< socket_;
		}
		return address;
	}

	SocketAddress Win32Socket::GetRemoteAddress() const {
		sockaddr_storage addr = {0};
		socklen_t addrlen = sizeof(addr);
		int result = ::getpeername(socket_, reinterpret_cast<sockaddr*>(&addr),
			&addrlen);
		SocketAddress address;
		if (result >= 0) {
			SocketAddressFromSockAddrStorage(addr, &address);
		} else {
			LOG(LS_WARNING) << "GetRemoteAddress: unable to get remote addr, socket="
				<< socket_;
		}
		return address;
	}

	int Win32Socket::GetError() const{
		return error_;
	}

	void Win32Socket::SetError(int error){
		error_ = error;
	}

	Socket::ConnState Win32Socket::GetState() const {
		return state_;
	}

	int Win32Socket::EstimateMTU(unsigned short* mtu) {
		LOG(LERROR) << "Win32Socket::EstimateMTU dose not implemented";
		return 0;
	}

	int Win32Socket::GetOption(Option opt, int* value) {
		int slevel;
		int sopt;
		if (TranslateOption(opt, &slevel, &sopt) == -1)
			return -1;

		char* p = reinterpret_cast<char*>(value);
		int optlen = sizeof(value);
		return ::getsockopt(socket_, slevel, sopt, p, &optlen);
	}

	int Win32Socket::SetOption(Option opt, int value) {
		int slevel;
		int sopt;
		if (TranslateOption(opt, &slevel, &sopt) == -1)
			return -1;

		const char* p = reinterpret_cast<const char*>(&value);
		return ::setsockopt(socket_, slevel, sopt, p, sizeof(value));
	}

	int Win32Socket::TranslateOption(Option opt, int* slevel, int* sopt) {
		switch (opt) {
		case OPT_DONTFRAGMENT:
			*slevel = IPPROTO_IP;
			*sopt = IP_DONTFRAGMENT;
			break;
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

	bool Win32Socket::HandleClosed(int close_error) {
		// WM_CLOSE will be received before all data has been read, so we need to
		// hold on to it until the read buffer has been drained.
		char ch;
		closing_ = true;
		close_error_ = close_error;
		return (::recv(socket_, &ch, 1, MSG_PEEK) <= 0);
	}

	bool Win32Socket::SetAsync(int events) {
		if (NULL == sink_) {
			CreateSink();
			ASSERT(NULL != sink_);
		}

		// start the async select
		if (WSAAsyncSelect(socket_, sink_->handle(), WM_SOCKETNOTIFY, events)
			== SOCKET_ERROR) {
				UpdateLastError();
				Close();
				return false;
		}

		return true;
	}

	void Win32Socket::CreateSink() {
		ASSERT(NULL == sink_);

		// Create window
		sink_ = new EventSink(this);
		sink_->Create(NULL, L"EventSink", 0, 0, 0, 0, 10, 10);
	}

	void Win32Socket::UpdateLastError() {
		error_ = WSAGetLastError();
	}

	void Win32Socket::PostClosed() {
		// If we see that the buffer is indeed drained, then send the close.
		closing_ = false;
		::PostMessage(sink_->handle(), WM_SOCKETNOTIFY,
			socket_, WSAMAKESELECTREPLY(FD_CLOSE, close_error_));
	}

	int Win32Socket::DoConnect(const SocketAddress& addr) {
		if ((socket_ == INVALID_SOCKET) && !CreateT(addr.family(), SOCK_STREAM)) {
			return SOCKET_ERROR;
		}
		if (!SetAsync(FD_READ | FD_WRITE | FD_CONNECT | FD_CLOSE)) {
			return SOCKET_ERROR;
		}

		sockaddr_storage saddr = {0};
		size_t len = addr.ToSockAddrStorage(&saddr);
		//connect_time_ = Time();
		int result = connect(socket_,
			reinterpret_cast<SOCKADDR*>(&saddr),
			static_cast<int>(len));
		if (result != SOCKET_ERROR) {
			state_ = CS_CONNECTED;
		} else {
			int code = WSAGetLastError();
			if (code == WSAEWOULDBLOCK) {
				state_ = CS_CONNECTING;
			} else {
				ReportWSAError("WSAAsync:connect", code, addr);
				error_ = code;
				Close();
				return SOCKET_ERROR;
			}
		}
		addr_ = addr;

		return 0;
	}

	void Win32Socket::OnSocketNotify(SOCKET socket, int event, int error) {
		if (socket != socket_)
			return;

		error_ = error;
		switch (event) {
		case FD_CONNECT:
			if (error != ERROR_SUCCESS) {
				ReportWSAError("WSAAsync:connect notify", error, addr_);
#ifdef _DEBUG
				int32 duration = 0/*TimeSince(connect_time_)*/;
				LOG(LS_INFO) << "WSAAsync:connect error (" << duration
					<< " ms), faking close";
#endif
				state_ = CS_CLOSED;
				// If you get an error connecting, close doesn't really do anything
				// and it certainly doesn't send back any close notification, but
				// we really only maintain a few states, so it is easiest to get
				// back into a known state by pretending that a close happened, even
				// though the connect event never did occur.
				SignalCloseEvent(this, error);
			} else {
#ifdef _DEBUG
				int32 duration = 0/*TimeSince(connect_time_)*/;
				LOG(LS_INFO) << "WSAAsync:connect (" << duration << " ms)";
#endif
				state_ = CS_CONNECTED;
				SignalConnectEvent(this);
			}
			break;
		
		case FD_ACCEPT:
		case FD_READ:
			if (error != ERROR_SUCCESS) {
				ReportWSAError("WSAAsync:read notify", error, addr_);
			} else {
				SignalReadEvent(this);
			}
			break;

		case FD_WRITE:
			if (error != ERROR_SUCCESS) {
				ReportWSAError("WSAAsync:write notify", error, addr_);
			} else {
				SignalWriteEvent(this);
			}
			break;

		case FD_CLOSE:
			if (HandleClosed(error)) {
				ReportWSAError("WSAAsync:close notify", error, addr_);
				state_ = CS_CLOSED;
				SignalCloseEvent(this, error);
			}
			break;
		}
	}

	void Win32Socket::OnDnsNotify(HANDLE task, int error) {
		if (!dns_ || dns_->handle != task)
			return;

		uint32 ip = 0;
		if (error == 0) {
			hostent* pHost = reinterpret_cast<hostent*>(dns_->buffer);
			uint32 net_ip = *reinterpret_cast<uint32*>(pHost->h_addr_list[0]);
			ip = NetworkToHost32(net_ip);
		}

		LOG(LS_INFO) << "(" << SocketAddress::IPToString(ip)
			<< ", " << error << ")";

		if (error == 0) {
			SocketAddress address(ip, dns_->port);
			error = DoConnect(address);
		} else {
			Close();
		}

		if (error) {
			error_ = error;
			SignalCloseEvent(this, error_);
		} else {
			delete dns_;
			dns_ = NULL;
		}
	}

} //mamespace base