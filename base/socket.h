#ifndef __EYOU_DANDELION_SOCKET_H__
#define __EYOU_DANDELION_SOCKET_H__

#include <errno.h>

#include "base/win32.h"
#include "base/socketaddress.h"

#ifdef WIN32
#undef EWOULDBLOCK  // Remove errno.h's definition for each macro below.
#define EWOULDBLOCK WSAEWOULDBLOCK
#undef EINPROGRESS
#define EINPROGRESS WSAEINPROGRESS
#undef EALREADY
#define EALREADY WSAEALREADY
#undef ENOTSOCK
#define ENOTSOCK WSAENOTSOCK
#undef EDESTADDRREQ
#define EDESTADDRREQ WSAEDESTADDRREQ
#undef EMSGSIZE
#define EMSGSIZE WSAEMSGSIZE
#undef EPROTOTYPE
#define EPROTOTYPE WSAEPROTOTYPE
#undef ENOPROTOOPT
#define ENOPROTOOPT WSAENOPROTOOPT
#undef EPROTONOSUPPORT
#define EPROTONOSUPPORT WSAEPROTONOSUPPORT
#undef ESOCKTNOSUPPORT
#define ESOCKTNOSUPPORT WSAESOCKTNOSUPPORT
#undef EOPNOTSUPP
#define EOPNOTSUPP WSAEOPNOTSUPP
#undef EPFNOSUPPORT
#define EPFNOSUPPORT WSAEPFNOSUPPORT
#undef EAFNOSUPPORT
#define EAFNOSUPPORT WSAEAFNOSUPPORT
#undef EADDRINUSE
#define EADDRINUSE WSAEADDRINUSE
#undef EADDRNOTAVAIL
#define EADDRNOTAVAIL WSAEADDRNOTAVAIL
#undef ENETDOWN
#define ENETDOWN WSAENETDOWN
#undef ENETUNREACH
#define ENETUNREACH WSAENETUNREACH
#undef ENETRESET
#define ENETRESET WSAENETRESET
#undef ECONNABORTED
#define ECONNABORTED WSAECONNABORTED
#undef ECONNRESET
#define ECONNRESET WSAECONNRESET
#undef ENOBUFS
#define ENOBUFS WSAENOBUFS
#undef EISCONN
#define EISCONN WSAEISCONN
#undef ENOTCONN
#define ENOTCONN WSAENOTCONN
#undef ESHUTDOWN
#define ESHUTDOWN WSAESHUTDOWN
#undef ETOOMANYREFS
#define ETOOMANYREFS WSAETOOMANYREFS
#undef ETIMEDOUT
#define ETIMEDOUT WSAETIMEDOUT
#undef ECONNREFUSED
#define ECONNREFUSED WSAECONNREFUSED
#undef ELOOP
#define ELOOP WSAELOOP
#undef ENAMETOOLONG
#define ENAMETOOLONG WSAENAMETOOLONG
#undef EHOSTDOWN
#define EHOSTDOWN WSAEHOSTDOWN
#undef EHOSTUNREACH
#define EHOSTUNREACH WSAEHOSTUNREACH
#undef ENOTEMPTY
#define ENOTEMPTY WSAENOTEMPTY
#undef EPROCLIM
#define EPROCLIM WSAEPROCLIM
#undef EUSERS
#define EUSERS WSAEUSERS
#undef EDQUOT
#define EDQUOT WSAEDQUOT
#undef ESTALE
#define ESTALE WSAESTALE
#undef EREMOTE
#define EREMOTE WSAEREMOTE
#undef SOCKET_EACCES
#define SOCKET_EACCES WSAEACCES
#endif  // WIN32

namespace base {

inline bool IsBlockingError(int e) {
	return (e == EWOULDBLOCK) || (e == EAGAIN) || (e == EINPROGRESS);
}

	class Socket {
	public:
		virtual ~Socket() {};

		// Returns the address to which the socket is bound.  If the socket is not
		// bound, then the any-address is returned.
		virtual SocketAddress GetLocalAddress() const = 0;

		// Returns the address to which the socket is connected.  If the socket is
		// not connected, then the any-address is returned.
		virtual SocketAddress GetRemoteAddress() const = 0;

		virtual int Bind(const SocketAddress& addr) = 0;
		virtual int Connect(const SocketAddress& addr) = 0;
		virtual int Send(const void *pv,size_t cb) = 0;
		virtual int SendTo(const void *pv, size_t cb, const SocketAddress& addr) = 0;
		virtual int Recv(void *pv,size_t cb) = 0;
		virtual int RecvFrom(void *pv, size_t cb, SocketAddress *paddr) = 0;
		virtual int Listen(int backlog) = 0;
		virtual Socket *Accept(SocketAddress *paddr) = 0;
		virtual int Close() = 0;
		virtual int GetError() const = 0;
		virtual void SetError(int error) = 0;
		inline bool IsBlocking() const {
			return IsBlockingError(GetError());
		}

		enum ConnState {
			CS_CLOSED,
			CS_CONNECTING,
			CS_CONNECTED
		};
		virtual ConnState GetState() const = 0;

		virtual int EstimateMTU(unsigned short* mtu) = 0;

		enum Option {
			OPT_DONTFRAGMENT,
			OPT_RCVBUF,      // receive buffer size
			OPT_SNDBUF,      // send buffer size
			OPT_NODELAY,     // whether Nagle algorithm is enabled
			OPT_IPV6_V6ONLY  // Whether the socket is IPv6 only.
		};
		virtual int GetOption(Option opt, int* value) = 0;
		virtual int SetOption(Option opt, int value) = 0;
	protected:
		Socket() {}
	private:
	};
}	//namespace base

#endif	//__EYOU_DANDELION_SOCKET_H__