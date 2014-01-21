#ifndef __H_BASE_SOCKET_FACTORY_H__
#define __H_BASE_SOCKET_FACTORY_H__

#include "base/socket.h"
#include "base/asyncsocket.h"

namespace base {
	class socketfactory {
	public:
		virtual ~socketfactory() {
		}

		//Return a new socket for blocking communication. 
		//The type can be SOCKET_DGRAM and SOCKET_STREAM
		virtual Socket* CreateSocket(int type) = 0;
		virtual Socket* CreateSocket(int family,int type) = 0;
		//Return a new sockeet for nonblocking communication
		virtual AsyncSocket* CreateAsyncSocket(int type) = 0;
		virtual AsyncSocket* CreateAsyncSocket(int family,int type) = 0;
	};
}	// namespace

#endif	//__H_BASE_SOCKET_FACTORY_H__