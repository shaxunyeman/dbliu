#ifndef __H_BASE_TIMEUTILS_H__
#define __H_BASE_TIMEUTILS_H__

#ifdef WIN32
#include <time.h>
#endif	//WIN32

#include "base/basictypes.h"

namespace base {
	static const int64 kNumMillisecsPerSec = 1000;
	static const int64 kNumMicrosecsPerSec = 1000000;
	static const int64 kNumNanosecsPerSec = 1000000000;

	static const int64 kNumMicrosecsPerMillisec = kNumMicrosecsPerSec /
		kNumMillisecsPerSec;
	static const int64 kNumNanosecsPerMillisec =  kNumNanosecsPerSec /
		kNumMillisecsPerSec;

	typedef uint32 TimeStamp;

	uint32 Time();
	uint64 TimeNanos();
	
	// Returns a future timestamp, 'elapsed' milliseconds from now.
	uint32 TimeAfter(int32 elapsed);

	// Comparisons between time values, which can wrap around.
	bool TimeIsBetween(uint32 earlier, uint32 middle, uint32 later);  // Inclusive
	bool TimeIsLaterOrEqual(uint32 earlier, uint32 later);  // Inclusive
	bool TimeIsLater(uint32 earlier, uint32 later);  // Exclusive

	// Returns the later of two timestamps.
	inline uint32 TimeMax(uint32 ts1, uint32 ts2) {
		return TimeIsLaterOrEqual(ts1, ts2) ? ts2 : ts1;
	}

	// Returns the earlier of two timestamps.
	inline uint32 TimeMin(uint32 ts1, uint32 ts2) {
		return TimeIsLaterOrEqual(ts1, ts2) ? ts1 : ts2;
	}

	// Number of milliseconds that would elapse between 'earlier' and 'later'
	// timestamps.  The value is negative if 'later' occurs before 'earlier'.
	int32 TimeDiff(uint32 later, uint32 earlier);

	// The number of milliseconds that have elapsed since 'earlier'.
	inline int32 TimeSince(uint32 earlier) {
		return TimeDiff(Time(), earlier);
	}

	// The number of milliseconds that will elapse between now and 'later'.
	inline int32 TimeUntil(uint32 later) {
		return TimeDiff(later, Time());
	}

}	//namespace base

#endif	//__H_BASE_TIMEUTILS_H__