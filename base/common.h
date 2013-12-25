#ifndef __H_EYOU_DANDELION_COMMON_H__
#define __H_EYOU_DANDELION_COMMON_H__


#if defined(_MSC_VER)
// warning C4355: 'this' : used in base member initializer list
#pragma warning(disable:4355)
#endif

#define ARRAY_SIZE(x) (static_cast<int>(sizeof(x) / sizeof(x[0])))

#ifndef ENABLE_DEBUG
#define ENABLE_DEBUG _DEBUG
#endif  // !defined(ENABLE_DEBUG)

#if ENABLE_DEBUG

namespace base {
	void Break();

	// LogAssert writes information about an assertion to the log
	void LogAssert(const char* function, const char* file, int line,
		const char* expression);

	inline bool Assert(bool result, const char* function, const char* file,
		int line, const char* expression) {
			if (!result) {
				LogAssert(function, file, line, expression);
				Break();
				return false;
			}
			return true;
	}

} //namespace base

#if defined(_MSC_VER) && _MSC_VER < 1300
#define __FUNCTION__ ""
#endif

#ifndef ASSERT
#define ASSERT(x) \
    (void)base::Assert((x), __FUNCTION__, __FILE__, __LINE__, #x)
#endif

#ifndef VERIFY
#define VERIFY(x) base::Assert((x), __FUNCTION__, __FILE__, __LINE__, #x)
#endif

#else  // !ENABLE_DEBUG

namespace base {

inline bool ImplicitCastToBool(bool result) { return result; }

}  // namespace base

#ifndef ASSERT
#define ASSERT(x) (void)0
#endif

#ifndef VERIFY
#define VERIFY(x) base::ImplicitCastToBool(x)
#endif

#endif  // !ENABLE_DEBUG

#endif	//__H_EYOU_DANDELION_COMMON_H__