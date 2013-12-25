#ifndef __EYOU_DANDELION_BASE_IPADDRESS_H__
#define __EYOU_DANDELION_BASE_IPADDRESS_H__

#include <ws2tcpip.h>	//in6_addr
#include <string>

#include "base/win32.h"
#include "base/byteorder.h"
#include "base/basictypes.h"

namespace base {
	class IPAddress {
	public:
		IPAddress()
		:family_(AF_UNSPEC){
			::memset(&u_,0,sizeof(u_));
		}

		explicit IPAddress(const in_addr &ip) 
			:family_(AF_INET){
				memset(&u_, 0, sizeof(u_));
				u_.ip4 = ip;
		}

		explicit IPAddress(const in6_addr &ip6) 
			:family_(AF_INET6) {
				u_.ip6 = ip6;
		}

		explicit IPAddress(uint32 ip_in_host_byte_order) 
			: family_(AF_INET) {
			memset(&u_, 0, sizeof(u_));
			u_.ip4.s_addr = HostToNetwork32(ip_in_host_byte_order);
		}

		IPAddress(const IPAddress &other) : family_(other.family_) {
			::memcpy(&u_, &other.u_, sizeof(u_));
		}

		~IPAddress() {}

		const IPAddress & operator=(const IPAddress &other) {
			family_ = other.family_;
			::memcpy(&u_, &other.u_, sizeof(u_));
			return *this;
		}

		int family() const { return family_; }
		in_addr ipv4_address() const;
		in6_addr ipv6_address() const;


		bool operator==(const IPAddress &other) const;
		bool operator!=(const IPAddress &other) const;
		bool operator <(const IPAddress &other) const;
		bool operator >(const IPAddress &other) const;

		uint32 v4AddressAsHostOrderInteger() const;

		// Returns this address as an IPv6 address.
		// Maps v4 addresses (as ::ffff:a.b.c.d), returns v6 addresses unchanged.
		IPAddress AsIPv6Address() const;
	protected:
	private:
		int family_;
		union {
			in_addr ip4;
			in6_addr ip6;
		}u_;
	};

	//////////////////////////////////////////////////////////////////////////////////////

	bool IPFromString(const std::string& str, IPAddress* out);
	bool IPIsUnspec(const IPAddress& ip);
	bool IPIsAny(const IPAddress& ip);
	bool IPIsLoopback(const IPAddress& ip);
	bool IPIsPrivate(const IPAddress& ip);
}	//namespace base

#endif	//__EYOU_DANDELION_BASE_IPADDRESS_H__