#include "base/ssladapter.h"

#if SSL_USE_OPENSSL
#include "base/openssladapter.h"
#endif //SSL_USE_OPENSSL

namespace base {

	SSLAdapter* SSLAdapter::Create(AsyncSocket* socket) {
#if SSL_USE_OPENSSL
		return new OpenSSLAdapter(socket);
#else //!SSL_USE_OPENSSL
		return NULL;
#endif	//SSL_USE_OPENSSL
	}

	////////////////////////////////////////////////////////////////////////
#if SSL_USE_OPENSSL
	bool InitializeSSL(VerificationCallback callback) {
		return OpenSSLAdapter::InitializeSSL(callback);
	}

	bool InitializeSSLThread() {
		return OpenSSLAdapter::InitializeSSLThread();
	}

	bool CleanupSSL() {
		return OpenSSLAdapter::CleanupSSL();
	}

#else	//!SSL_USE_OPENSSL
	bool InitializeSSL(VerificationCallback callback) {
		return true;
	}

	bool InitializeSSLThread() {
		return true;
	}

	bool CleanupSSL() {
		return true;
	}
#endif
}	//namespace base
