#ifndef __H_BASE_SOCKETSERVER_H__
#define __H_BASE_SOCKETSERVER_H__

#include "base/socketfactory.h"

namespace base {
	
	class MessageQueue;

	class SocketServer : public socketfactory {
	public:
		virtual void SetMessageQueue(MessageQueue* queue) {}
		virtual bool Wait(int cms,bool process_io) = 0;
		virtual void WakeUp() = 0;
	private:
	};
}

#endif