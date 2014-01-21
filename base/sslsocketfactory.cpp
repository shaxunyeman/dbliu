#include "base/sslsocketfactory.h"
#include "base/logging.h"
#include "base/asyncsocket.h"
#include "base/openssladapter.h"

namespace base {
	SslSocketFactory::SslSocketFactory(socketfactory *factory)
	:factory_(factory)
	,ignore_bad_cert_(false) {
	}

	SslSocketFactory::~SslSocketFactory() {
	}

	// socketfactory interface
	Socket* SslSocketFactory::CreateSocket(int type) {
		return CreateSocket(AF_INET,type);
	}

	Socket* SslSocketFactory::CreateSocket(int family,int type) {
		return factory_->CreateSocket(family,type);
	}

	AsyncSocket* SslSocketFactory::CreateAsyncSocket(int type) {
		return CreateAsyncSocket(AF_INET,type);
	}

	AsyncSocket* SslSocketFactory::CreateAsyncSocket(int family,int type) {
		return CreateAsyncSocketImpl(family,type);
	}

	AsyncSocket *SslSocketFactory::CreateAsyncSocketImpl(int family,int type) {
		AsyncSocket* socket = factory_->CreateAsyncSocket(family,type);
		if (!socket)
			return NULL;

		if (!hostname_.empty()) {
			if (SSLAdapter *ssl_adapter = SSLAdapter::Create(socket)) {
				ssl_adapter->set_ignore_bad_cert(ignore_bad_cert_);
				ssl_adapter->StartSSL(hostname_.c_str(),true);
				socket = ssl_adapter;
			} else {
				LOG(LS_ERROR) << "SSL unavailable";
			}
		}

		return socket;
	}
}	// namespace base