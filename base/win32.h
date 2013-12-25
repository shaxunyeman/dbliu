#ifndef __H_EYOU_DANDELION_WIN32_H__
#define __H_EYOU_DANDELION_WIN32_H__

#include <WinSock2.h>
#include <Windows.h>

#include "base/basictypes.h"

namespace base {
	int inet_pton(int af, const char* src, void *dst);
}	//namespace base

#endif	//__H_EYOU_DANDELION_WIN32_H__