#include <locale>

#include "base/stringutils.h"

namespace base {
	bool memory_check(const void* memory, int c, size_t count) {
		const char* char_memory = static_cast<const char*>(memory);
		char char_c = static_cast<char>(c);
		for (size_t i = 0; i < count; ++i) {
			if (char_memory[i] != char_c) {
				return false;
			}
		}
		return true;
	}

	bool string_match(const char* target, const char* pattern) {
		while (*pattern) {
			if (*pattern == '*') {
				if (!*++pattern) {
					return true;
				}
				while (*target) {
					if ((toupper(*pattern) == toupper(*target))
						&& string_match(target + 1, pattern + 1)) {
							return true;
					}
					++target;
				}
				return false;
			} else {
				if (toupper(*pattern) != toupper(*target)) {
					return false;
				}
				++target;
				++pattern;
			}
		}
		return !*target;
	}

}