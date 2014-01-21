#ifndef __H_EYOU_DANDELION_BASE_SOCKETADAPTER_H__
#define __H_EYOU_DANDELION_BASE_SOCKETADAPTER_H__

#include "base/asyncsocket.h"
#include "base/constructormagic.h"

namespace base {
	class BufferReadAdapter : public  AsyncSocketAdapter {
	public:
		BufferReadAdapter(AsyncSocket* socket,size_t buffer_size);
		virtual ~BufferReadAdapter(void);

		virtual int Send(const void* pv, size_t cb);
		virtual int Recv(void* pv, size_t cb);
	protected:
		int DirectSend(const void* pv, size_t cb) {
			return  AsyncSocketAdapter::Send(pv, cb);
		}

		void BufferInput(bool on = true);
		virtual void ProcessInput(char* data, size_t* len) = 0;
		virtual void OnReadEvent(AsyncSocket * socket);
	private:
		char *buffer_;
		size_t buffer_size_,data_len_;
		bool buffering_;
		DISALLOW_EVIL_CONSTRUCTORS(BufferReadAdapter);
	};

	//////////////////////////////////////////////////////////////////////////////////////////
	//AsyncSSLSocket
	//////////////////////////////////////////////////////////////////////////////////////////
	class AsyncSSLSocket : public BufferReadAdapter {
	public:
		explicit AsyncSSLSocket(AsyncSocket* socket);
		virtual int Connect(const SocketAddress& addr);
	protected:
		virtual void OnConnectEvent(AsyncSocket* socket);
		virtual void ProcessInput(char* data, size_t* len);
		DISALLOW_EVIL_CONSTRUCTORS(AsyncSSLSocket);
	private:
	};
}	//namespace base

#endif	//__H_EYOU_DANDELION_BASE_SOCKETADAPTER_H__