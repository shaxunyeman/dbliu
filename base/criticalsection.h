#ifndef __H_EYOU_DANDELION_CRITICALSECTION_H__
#define __H_EYOU_DANDELION_CRITICALSECTION_H__

#include <Windows.h>

namespace base {
	class CriticalSection {
	public:
		CriticalSection() {
			InitializeCriticalSection(&crit_);
			// Windows docs say 0 is not a valid thread id
		}
		~CriticalSection() {
			DeleteCriticalSection(&crit_);
		}
		void Enter() {
			EnterCriticalSection(&crit_);
		}
		bool TryEnter() {
			if (TryEnterCriticalSection(&crit_) != FALSE) {
				return true;
			}
			return false;
		}
		void Leave() {
			LeaveCriticalSection(&crit_);
		}

	private:
		CRITICAL_SECTION crit_;
	};

	class CritScope {
	public:
		explicit CritScope(CriticalSection *pcrit) {
			pcrit_ = pcrit;
			pcrit_->Enter();
		}
		~CritScope() {
			pcrit_->Leave();
		}
	private:
		CritScope() {}
		CriticalSection *pcrit_;
	};
}	//namespace base

#endif	//__H_EYOU_DANDELION_CRITICALSECTION_H__