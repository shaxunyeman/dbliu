#include "base/ipaddress.h"

namespace base {

	static const in6_addr kV4MappedPrefix = {{{0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0xFF, 0xFF, 0}}};

	uint32 IPAddress::v4AddressAsHostOrderInteger() const {
		if (family_ == AF_INET) {
			return NetworkToHost32(u_.ip4.s_addr);
		} else {
			return 0;
		}
	}

	bool IPAddress::operator==(const IPAddress &other) const {
		if (family_ != other.family_) {
			return false;
		}
		if (family_ == AF_INET) {
			return memcmp(&u_.ip4, &other.u_.ip4, sizeof(u_.ip4)) == 0;
		}
		if (family_ == AF_INET6) {
			return memcmp(&u_.ip6, &other.u_.ip6, sizeof(u_.ip6)) == 0;
		}
		return family_ == AF_UNSPEC;
	}

	bool IPAddress::operator!=(const IPAddress &other) const {
		return !((*this) == other);
	}

	bool IPAddress::operator >(const IPAddress &other) const {
		return (*this) != other && !((*this) < other);
	}

	bool IPAddress::operator <(const IPAddress &other) const {
		// IPv4 is 'less than' IPv6
		if (family_ != other.family_) {
			if (family_ == AF_UNSPEC) {
				return true;
			}
			if (family_ == AF_INET && other.family_ == AF_INET6) {
				return true;
			}
			return false;
		}
		// Comparing addresses of the same family.
		switch (family_) {
		case AF_INET: {
			return NetworkToHost32(u_.ip4.s_addr) <
				NetworkToHost32(other.u_.ip4.s_addr);
					  }
		case AF_INET6: {
			return memcmp(&u_.ip6.s6_addr, &other.u_.ip6.s6_addr, 16) < 0;
					   }
		}
		// Catches AF_UNSPEC and invalid addresses.
		return false;
	}

	in6_addr IPAddress::ipv6_address() const {
		return u_.ip6;
	}

	in_addr IPAddress::ipv4_address() const {
		return u_.ip4;
	}

	IPAddress IPAddress::AsIPv6Address() const {
		if (family_ != AF_INET) {
			return *this;
		}
		in6_addr v6addr = kV4MappedPrefix;
		::memcpy(&v6addr.s6_addr[12], &u_.ip4.s_addr, sizeof(u_.ip4.s_addr));
		return IPAddress(v6addr);
	}


	////////////////////////////////////////////////////////////////////////////////
	bool IPFromString(const std::string& str, IPAddress* out) {
		if (!out) {
			return false;
		}
		in_addr addr;
		if (base::inet_pton(AF_INET, str.c_str(), &addr) == 0) {
			in6_addr addr6;
			if (base::inet_pton(AF_INET6, str.c_str(), &addr6) == 0) {
				*out = IPAddress();
				return false;
			}
			*out = IPAddress(addr6);
		} else {
			*out = IPAddress(addr);
		}
		return true; 
	}


	bool IsPrivateV4(uint32 ip_in_host_order) {
		return ((ip_in_host_order >> 24) == 127) ||
			((ip_in_host_order >> 24) == 10) ||
			((ip_in_host_order >> 20) == ((172 << 4) | 1)) ||
			((ip_in_host_order >> 16) == ((192 << 8) | 168)) ||
			((ip_in_host_order >> 16) == ((169 << 8) | 254));
	}

	bool IPIsUnspec(const IPAddress& ip) {
		return ip.family() == AF_UNSPEC;
	}

	bool IPIsAny(const IPAddress& ip) {
		switch (ip.family()) {
		case AF_INET:
			return ip == IPAddress(INADDR_ANY);
		case AF_INET6:
			return ip == IPAddress(in6addr_any);
		case AF_UNSPEC:
			return false;
		}
		return false;
	}

	bool IPIsLoopback(const IPAddress& ip) {
		switch (ip.family()) {
		case AF_INET: {
			return ip == IPAddress(INADDR_LOOPBACK);
					  }
		case AF_INET6: {
			return ip == IPAddress(in6addr_loopback);
					   }
		}
		return false;
	}

	bool IPIsPrivate(const IPAddress& ip) {
		switch (ip.family()) {
		case AF_INET: {
			return IsPrivateV4(ip.v4AddressAsHostOrderInteger());
					  }
		case AF_INET6: {
			in6_addr v6 = ip.ipv6_address();
			return (v6.s6_addr[0] == 0xFE && v6.s6_addr[1] == 0x80) ||
				IPIsLoopback(ip);
					   }
		}
		return false;
	}
} //namespace base