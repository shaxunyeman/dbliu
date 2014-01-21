#ifndef __H_BASE_OPENSSLADPATER_H__
#define __H_BASE_OPENSSLADPATER_H__

#include "base/ssladapter.h"

typedef struct ssl_st SSL;
typedef struct ssl_ctx_st SSL_CTX;
typedef struct x509_store_ctx_st X509_STORE_CTX;

namespace base {

	class OpenSSLAdapter : public SSLAdapter {
	public:
		OpenSSLAdapter(AsyncSocket *socket);
		virtual ~OpenSSLAdapter();

		static bool InitializeSSL(VerificationCallback callback);
		static bool InitializeSSLThread();
		static bool CleanupSSL();

		virtual int StartSSL(const char* hostname, bool restartable);
		virtual int Send(const void* pv, size_t cb);
		virtual int Recv(void* pv, size_t cb);
		virtual int Close();

		// Note that the socket returns ST_CONNECTING while SSL is being negotiated.
		virtual ConnState GetState() const;

	protected:
		virtual void OnConnectEvent(AsyncSocket* socket);
		virtual void OnReadEvent(AsyncSocket* socket);
		virtual void OnWriteEvent(AsyncSocket* socket);
		virtual void OnCloseEvent(AsyncSocket* socket, int err);
	private:
		enum SSLState {
			SSL_NONE, SSL_WAIT, SSL_CONNECTING, SSL_CONNECTED, SSL_ERROR
		};

		int BeginSSL();
		int ContinueSSL();
		void Error(const char* context, int err, bool signal = true);
		void Cleanup();

		static bool VerifyServerName(SSL* ssl, const char* host,
										bool ignore_bad_cert);
		bool SSLPostConnectionCheck(SSL* ssl, const char* host);
#if _DEBUG
		static void SSLInfoCallback(const SSL* s, int where, int ret);
#endif  // !_DEBUG
		static int SSLVerifyCallback(int ok, X509_STORE_CTX* store);
		static VerificationCallback custom_verify_callback_;
		//friend class OpenSSLStreamAdapter;  // for custom_verify_callback_;

		static bool ConfigureTrustedRootCertificates(SSL_CTX* ctx);
		static SSL_CTX* SetupSSLContext();

		SSLState state_;
		bool ssl_read_needs_write_;
		bool ssl_write_needs_read_;
		// If true, socket will retain SSL configuration after Close.
		bool restartable_;

		SSL* ssl_;
		SSL_CTX* ssl_ctx_;
		std::string ssl_host_name_;

		bool custom_verification_succeeded_;

	};

}

#endif	//__H_BASE_OPENSSLADPATER_H__