#ifndef __H_EYOU_DANDELION_ASYNCSOCKET_H__
#define __H_EYOU_DANDELION_ASYNCSOCKET_H__

#include "base/socket.h"
#include "base/sigslot.h"

namespace base {

	class AsyncSocket : public Socket {
	public:
		AsyncSocket();
		virtual ~AsyncSocket();

		virtual AsyncSocket* Accept(SocketAddress* paddr) = 0;

		sigslot::signal1<AsyncSocket*> SignalReadEvent;        // ready to read
		sigslot::signal1<AsyncSocket*> SignalWriteEvent;       // ready to write
		sigslot::signal1<AsyncSocket*> SignalConnectEvent;     // connected
		sigslot::signal2<AsyncSocket*, int> SignalCloseEvent;  // closed
	protected:
	private:
	};

	class AsyncSocketAdapter : public AsyncSocket, public sigslot::has_slots<> {
	public:
		explicit AsyncSocketAdapter(AsyncSocket *socket);
		virtual ~AsyncSocketAdapter(void);
		void Attach(AsyncSocket* socket);

		// AsyncSocket Interface

		virtual SocketAddress GetLocalAddress() const {
			return socket_->GetLocalAddress();
		}

		virtual SocketAddress GetRemoteAddress() const {
			return socket_->GetRemoteAddress();
		}

		virtual int Bind(const SocketAddress& addr) {
			return socket_->Bind(addr);
		}

		virtual int Connect(const SocketAddress& addr) {
			return socket_->Connect(addr);
		}

		virtual int Send(const void *pv,size_t cb) {
			return socket_->Send(pv,cb);
		}

		virtual int SendTo(const void *pv, size_t cb, const SocketAddress& addr) {
			return socket_->SendTo(pv,cb,addr);
		}

		virtual int Recv(void *pv,size_t cb) {
			return socket_->Recv(pv,cb);
		}

		virtual int RecvFrom(void *pv, size_t cb, SocketAddress *paddr) {
			return socket_->RecvFrom(pv,cb,paddr);
		}

		virtual int Listen(int backlog) {
			return socket_->Listen(backlog);
		}

		virtual AsyncSocket *Accept(SocketAddress *paddr) {
			return socket_->Accept(paddr);
		}

		virtual int Close() {
			return socket_->Close();
		}

		virtual int GetError() const {
			return socket_->GetError();
		}

		virtual void SetError(int error) {
			socket_->SetError(error);
		}

		virtual ConnState GetState() const {
			return socket_->GetState();
		}

		virtual int EstimateMTU(unsigned short* mtu) {
			return socket_->EstimateMTU(mtu);
		}

		virtual int GetOption(Option opt, int* value) {
			return socket_->GetOption(opt, value);
		}

		virtual int SetOption(Option opt, int value) {
			return socket_->SetOption(opt, value);
		}

	protected:
		virtual void OnConnectEvent(AsyncSocket* socket) {
			SignalConnectEvent(this);
		}
		virtual void OnReadEvent(AsyncSocket* socket) {
			SignalReadEvent(this);
		}
		virtual void OnWriteEvent(AsyncSocket* socket) {
			SignalWriteEvent(this);
		}
		virtual void OnCloseEvent(AsyncSocket* socket, int err) {
			SignalCloseEvent(this, err);
		}
		AsyncSocket* socket_;
	private:

	};

} //namespace base

#endif