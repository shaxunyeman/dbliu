#ifndef __EYOU_DANDELION_LOGFILESTREAM_H__
#define __EYOU_DANDELION_LOGFILESTREAM_H__

#include <fstream>
#include <string>

#include "base/stream.h"

using namespace std;


namespace base {

	class LogFileStream : public StreamInterface {
	public:
		LogFileStream(const char *logfile);
		~LogFileStream(void);

		//StreamInterface
		int write(const char *data,unsigned int size);
		int read(char *buffer,unsigned int size);
	protected:
		LogFileStream() {};
		//get size of the logfile
		long tell_size();
		//clip logfile
		int clip(long logsize);
	private:
		string logfile_;
	};

}	//namespace base

#endif	//__EYOU_DANDELION_LOGFILESTREAM_H__