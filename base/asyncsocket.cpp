#include "base/asyncsocket.h"

namespace base {

	AsyncSocket::AsyncSocket() {

	}

	AsyncSocket::~AsyncSocket() {

	}

	//AsyncSocketAdapter
	AsyncSocketAdapter::AsyncSocketAdapter(AsyncSocket *socket)
	: socket_(NULL){
		Attach(socket);
	}

	AsyncSocketAdapter::~AsyncSocketAdapter(void) {
		delete socket_;
	}

	void AsyncSocketAdapter::Attach(AsyncSocket* socket) {
		socket_ = socket;
		if (socket_) {
			socket_->SignalConnectEvent.connect(this,
				&AsyncSocketAdapter::OnConnectEvent);
			socket_->SignalReadEvent.connect(this,
				&AsyncSocketAdapter::OnReadEvent);
			socket_->SignalWriteEvent.connect(this,
				&AsyncSocketAdapter::OnWriteEvent);
			socket_->SignalCloseEvent.connect(this,
				&AsyncSocketAdapter::OnCloseEvent);
		}
	}

} //namespace base