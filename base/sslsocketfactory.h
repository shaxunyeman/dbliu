#ifndef __H_BASE_SSLSOCKETFACTORY_H__
#define __H_BASE_SSLSOCKETFACTORY_H__

#include <string>

#include "base/socketfactory.h"

namespace base {
	class SslSocketFactory : public socketfactory {
	public:
		SslSocketFactory(socketfactory *factory);
		~SslSocketFactory();

		void UseSSL(const char* hostname) {
			hostname_ = hostname;
		}
		
		void DisableSSL() {
			hostname_.clear();
		}

		void SetIgnoreBadCert(bool ignore) {
			ignore_bad_cert_ = ignore;
		}

		bool ignore_bad_cert() const {
			return ignore_bad_cert_;
		}

		// socketfactory interface
		virtual Socket* CreateSocket(int type);
		virtual Socket* CreateSocket(int family,int type);
		virtual AsyncSocket* CreateAsyncSocket(int type);
		virtual AsyncSocket* CreateAsyncSocket(int family,int type);
	protected:
	private:
		AsyncSocket *CreateAsyncSocketImpl(int family,int type);

		socketfactory* factory_;
		std::string hostname_;
		bool ignore_bad_cert_;
	};
}	//namespace base

#endif	//__H_BASE_SSLSOCKETFACTORY_H__