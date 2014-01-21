#include "base/socketadapters.h"
#include "base/basictypes.h"
#include "base/common.h"
#include "base/logging.h"

namespace base {
	BufferReadAdapter::BufferReadAdapter(AsyncSocket* socket,size_t buffer_size)
	:AsyncSocketAdapter(socket)
	,buffer_size_(buffer_size)
	,data_len_(0)
	,buffering_(false){
		buffer_ = new char[buffer_size_];
	}

	BufferReadAdapter::~BufferReadAdapter(void) {
		delete []buffer_;
	}

	int BufferReadAdapter::Send(const void* pv, size_t cb) {
		if (buffering_) {
			socket_->SetError(EWOULDBLOCK);
			return -1;
		}

		return AsyncSocketAdapter::Send(pv,cb);
	}

	int BufferReadAdapter::Recv(void* pv, size_t cb) {
		if (buffering_) {
			socket_->SetError(EWOULDBLOCK);
			return -1;
		}

		size_t read = 0;
		if (data_len_) {
			read = _min(cb,data_len_);
			memcpy(pv,buffer_,read);
			data_len_ -= read;
			if (data_len_ > 0) {
				memmove(buffer_,buffer_ + read,data_len_);
			}
			pv = static_cast<char*>(pv) + read;
			cb -= read;
		}

		int res = AsyncSocketAdapter::Recv(pv, cb);
		if (res < 0)
			return res;
		return res + static_cast<int>(read);
	}

	void BufferReadAdapter::BufferInput(bool on) {
		buffering_ = on;
	}

	void BufferReadAdapter::OnReadEvent(AsyncSocket * socket) {
		ASSERT(socket == socket_);

		if (!buffering_) {
			AsyncSocketAdapter::OnReadEvent(socket);
			return;
		}

		if (data_len_ >= buffer_size_) {
			LOG(INFO) << "input buffer overflow";
			ASSERT(false);
			data_len_ = 0;
		}

		int len = socket_->Recv(buffer_ + data_len_,buffer_size_ - data_len_);
		if (len < 0) {
			LOG(LERROR) << "Recv";
			return;
		}

		data_len_ += len;
		ProcessInput(buffer_,&data_len_);
	}

	//////////////////////////////////////////////////////////////////////////////////////////
	//AsyncSSLSocket
	//////////////////////////////////////////////////////////////////////////////////////////
	// This is a SSL v2 CLIENT_HELLO message.
	// TODO: Should this have a session id? The response doesn't have a
	// certificate, so the hello should have a session id.
	static const uint8 kSslClientHello[] = {
		0x80, 0x46,                                            // msg len
		0x01,                                                  // CLIENT_HELLO
		0x03, 0x01,                                            // SSL 3.1
		0x00, 0x2d,                                            // ciphersuite len
		0x00, 0x00,                                            // session id len
		0x00, 0x10,                                            // challenge len
		0x01, 0x00, 0x80, 0x03, 0x00, 0x80, 0x07, 0x00, 0xc0,  // ciphersuites
		0x06, 0x00, 0x40, 0x02, 0x00, 0x80, 0x04, 0x00, 0x80,  //
		0x00, 0x00, 0x04, 0x00, 0xfe, 0xff, 0x00, 0x00, 0x0a,  //
		0x00, 0xfe, 0xfe, 0x00, 0x00, 0x09, 0x00, 0x00, 0x64,  //
		0x00, 0x00, 0x62, 0x00, 0x00, 0x03, 0x00, 0x00, 0x06,  //
		0x1f, 0x17, 0x0c, 0xa6, 0x2f, 0x00, 0x78, 0xfc,        // challenge
		0x46, 0x55, 0x2e, 0xb1, 0x83, 0x39, 0xf1, 0xea         //
	};

	// This is a TLSv1 SERVER_HELLO message.
	static const uint8 kSslServerHello[] = {
		0x16,                                            // handshake message
		0x03, 0x01,                                      // SSL 3.1
		0x00, 0x4a,                                      // message len
		0x02,                                            // SERVER_HELLO
		0x00, 0x00, 0x46,                                // handshake len
		0x03, 0x01,                                      // SSL 3.1
		0x42, 0x85, 0x45, 0xa7, 0x27, 0xa9, 0x5d, 0xa0,  // server random
		0xb3, 0xc5, 0xe7, 0x53, 0xda, 0x48, 0x2b, 0x3f,  //
		0xc6, 0x5a, 0xca, 0x89, 0xc1, 0x58, 0x52, 0xa1,  //
		0x78, 0x3c, 0x5b, 0x17, 0x46, 0x00, 0x85, 0x3f,  //
		0x20,                                            // session id len
		0x0e, 0xd3, 0x06, 0x72, 0x5b, 0x5b, 0x1b, 0x5f,  // session id
		0x15, 0xac, 0x13, 0xf9, 0x88, 0x53, 0x9d, 0x9b,  //
		0xe8, 0x3d, 0x7b, 0x0c, 0x30, 0x32, 0x6e, 0x38,  //
		0x4d, 0xa2, 0x75, 0x57, 0x41, 0x6c, 0x34, 0x5c,  //
		0x00, 0x04,                                      // RSA/RC4-128/MD5
		0x00                                             // null compression
	};


	AsyncSSLSocket::AsyncSSLSocket(AsyncSocket* socket)
	:BufferReadAdapter(socket,1024){
	}
	
	int AsyncSSLSocket::Connect(const SocketAddress& addr) {
		BufferInput(true);
		return BufferReadAdapter::Connect(addr);
	}

	void AsyncSSLSocket::OnConnectEvent(AsyncSocket* socket) {
		VERIFY(sizeof(kSslClientHello) == DirectSend(kSslClientHello,sizeof(kSslClientHello)));
	}

	void AsyncSSLSocket::ProcessInput(char* data, size_t* len) {
		if (*len < sizeof(kSslServerHello))
			return;

		if (memcmp(kSslServerHello, data, sizeof(kSslServerHello)) != 0) {
			Close();
			SignalCloseEvent(this, 0);  // TODO: error code?
			return;
		}

		*len -= sizeof(kSslServerHello);
		if (*len > 0) {
			memmove(data, data + sizeof(kSslServerHello), *len);
		}

		bool remainder = (*len > 0);
		BufferInput(false);
		SignalConnectEvent(this);

		// FIX: if SignalConnect causes the socket to be destroyed, we are in trouble
		if (remainder)
			SignalReadEvent(this);
	}

} //namespace base