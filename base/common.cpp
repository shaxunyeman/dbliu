#include "base/common.h"
#include "base/win32.h"
#include "base/logging.h"

namespace base {
	void Break() {
#if WIN32
		::DebugBreak();
#elif OSX  // !WIN32
		::Debugger();
#else // !OSX && !WIN32
#if _DEBUG_HAVE_BACKTRACE
		OutputTrace();
#endif
		abort();
#endif // !OSX && !WIN32
	}

	void LogAssert(const char * function, const char * file, int line,
		const char * expression) {
			// TODO - if we put hooks in here, we can do a lot fancier logging
			LOG(LS_ERROR) << file << "(" << line << ")" << ": ASSERT FAILED: "
				<< expression << " @ " << function;
	}

}	//namespace base