#include "base/win32.h"

#include <winsock2.h>
#include <ws2tcpip.h>
#include <algorithm>

#include <string.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>

#include "base/basictypes.h"
#include "base/byteorder.h"
#include "base/common.h"
#include "base/logging.h"

namespace base {

	static int inet_pton_v4(const char* src, void* dst) {
		const int kIpv4AddressSize = 4;
		int found = 0;
		const char* src_pos = src;
		unsigned char result[kIpv4AddressSize] = {0};

		while (*src_pos != '\0') {
			// strtol won't treat whitespace characters in the begining as an error,
			// so check to ensure this is started with digit before passing to strtol.
			if (!isdigit(*src_pos)) {
				return 0;
			}
			char* end_pos;
			long value = strtol(src_pos, &end_pos, 10);
			if (value < 0 || value > 255 || src_pos == end_pos) {
				return 0;
			}
			++found;
			if (found > kIpv4AddressSize) {
				return 0;
			}
			result[found - 1] = static_cast<unsigned char>(value);
			src_pos = end_pos;
			if (*src_pos == '.') {
				// There's more.
				++src_pos;
			} else if (*src_pos != '\0') {
				// If it's neither '.' nor '\0' then return fail.
				return 0;
			}
		}
		if (found != kIpv4AddressSize) {
			return 0;
		}
		memcpy(dst, result, sizeof(result));
		return 1;

	}

	static int inet_pton_v6(const char* src, void* dst) {
		return 0;
	}

	static int win32_inet_pton(int af, const char* src, void* dst) {
		if (!src || !dst)
			return 0;

		if (af == AF_INET)
			return inet_pton_v4(src,dst);
		else if (af == AF_INET6)
			return inet_pton_v6(src,dst);

		return -1;
	}

	int inet_pton(int af, const char* src, void *dst) {
#ifdef WIN32
		return win32_inet_pton(af,src,dst);
#else
		return ::inet_pton(af,src,dst);
#endif
	}
}	//namespace base