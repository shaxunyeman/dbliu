#include "base/timeutils.h"


#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <mmsystem.h>
#endif

#include "base/common.h"
#include "base/timeutils.h"

#define EFFICIENT_IMPLEMENTATION 1

namespace base {

	const uint32 LAST = 0xFFFFFFFF;
	const uint32 HALF = 0x80000000;

	uint32 Time() {
		return static_cast<uint32>(TimeNanos() / kNumNanosecsPerMillisec);
	}

	uint64 TimeNanos() {
		int64 ticks = 0;
#if defined (WIN32)
		static volatile LONG last_timegettime = 0;
		static volatile int64 num_wrap_timegettime = 0;
		volatile LONG* last_timegettime_ptr = &last_timegettime;
		DWORD now = timeGetTime();
		// Atomically update the last gotten time
		DWORD old = InterlockedExchange(last_timegettime_ptr, now);
		if (now < old) {
			// If now is earlier than old, there may have been a race between
			// threads.
			// 0x0fffffff ~3.1 days, the code will not take that long to execute
			// so it must have been a wrap around.
			if (old > 0xf0000000 && now < 0x0fffffff) {
				num_wrap_timegettime++;
			}
		}
		ticks = now + (num_wrap_timegettime << 32);
		// TODO: Calculate with nanosecond precision.  Otherwise, we're just
		// wasting a multiply and divide when doing Time() on Windows.
		ticks = ticks * kNumNanosecsPerMillisec;
#endif
		return ticks;
	}

	uint32 TimeAfter(int32 elapsed) {
		ASSERT(elapsed >= 0);
		ASSERT(static_cast<uint32>(elapsed) < HALF);
		return Time() + elapsed;
	}

	bool TimeIsBetween(uint32 earlier, uint32 middle, uint32 later) {
		if (earlier <= later) {
			return ((earlier <= middle) && (middle <= later));
		} else {
			return !((later < middle) && (middle < earlier));
		}
	}

	bool TimeIsLaterOrEqual(uint32 earlier, uint32 later) {
#if EFFICIENT_IMPLEMENTATION
		int32 diff = later - earlier;
		return (diff >= 0 && static_cast<uint32>(diff) < HALF);
#else
		const bool later_or_equal = TimeIsBetween(earlier, later, earlier + HALF);
		return later_or_equal;
#endif
	}

	bool TimeIsLater(uint32 earlier, uint32 later) {
#if EFFICIENT_IMPLEMENTATION
		int32 diff = later - earlier;
		return (diff > 0 && static_cast<uint32>(diff) < HALF);
#else
		const bool earlier_or_equal = TimeIsBetween(later, earlier, later + HALF);
		return !earlier_or_equal;
#endif
	}

	int32 TimeDiff(uint32 later, uint32 earlier) {
#if EFFICIENT_IMPLEMENTATION
		return later - earlier;
#else
		const bool later_or_equal = TimeIsBetween(earlier, later, earlier + HALF);
		if (later_or_equal) {
			if (earlier <= later) {
				return static_cast<long>(later - earlier);
			} else {
				return static_cast<long>(later + (LAST - earlier) + 1);
			}
		} else {
			if (later <= earlier) {
				return -static_cast<long>(earlier - later);
			} else {
				return -static_cast<long>(earlier + (LAST - later) + 1);
			}
		}
#endif
	}
}