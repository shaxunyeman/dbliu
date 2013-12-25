#ifndef __H_BASE_STRINGUTILS_H__
#define __H_BASE_STRINGUTILS_H__

namespace base {
	// Complement to memset.  Verifies memory consists of count bytes of value c.
	bool memory_check(const void* memory, int c, size_t count);

	// Determines whether the simple wildcard pattern matches target.
	// Alpha characters in pattern match case-insensitively.
	// Asterisks in pattern match 0 or more characters.
	// Ex: string_match("www.TEST.GOOGLE.COM", "www.*.com") -> true
	bool string_match(const char* target, const char* pattern);

}

#endif	//__H_BASE_STRINGUTILS_H__