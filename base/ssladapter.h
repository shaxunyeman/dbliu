#ifndef __H_BASE_SSLADAPTER_H__
#define __H_BASE_SSLADAPTER_H__

#include "base/asyncsocket.h"

namespace base {
	class SSLAdapter : public AsyncSocketAdapter {
	public:
		explicit SSLAdapter(AsyncSocket *socket)
			: AsyncSocketAdapter(socket), ignore_bad_cert_(false){
		}

		bool ignore_bad_cert() const { return ignore_bad_cert_; }
		void set_ignore_bad_cert(bool ignore) { ignore_bad_cert_ = ignore; }

		// StartSSL returns 0 if successful.
		// If StartSSL is called while the socket is closed or connecting, the SSL
		// negotiation will begin as soon as the socket connects.
		virtual int StartSSL(const char* hostname, bool restartable) = 0;

		// Create the default SSL adapter for this platform
		static SSLAdapter* Create(AsyncSocket* socket);
	protected:
	private:
		// If true, the server certificate need not match the configured hostname.
		bool ignore_bad_cert_;
	};

	///////////////////////////////////////////////////////////////////////////////

	typedef bool (*VerificationCallback)(void* cert);

	// Call this on the main thread, before using SSL.
	// Call CleanupSSLThread when finished with SSL.
	bool InitializeSSL(VerificationCallback callback = NULL);

	// Call to initialize additional threads.
	bool InitializeSSLThread();

	// Call to cleanup additional threads, and also the main thread.
	bool CleanupSSL();

}	//namespace base

#endif	//__H_BASE_SSLADAPTER_H__