#ifndef __EYOU_DANDELION_LOGGING_H__
#define __EYOU_DANDELION_LOGGING_H__

#include <sstream>
#include <string>
#include <vector>

#include "base/criticalsection.h"

using namespace std;


enum LoggingSeverity {
	LS_SENSITIVE, 
	LS_VERBOSE, 
	LS_INFO, 
	LS_WARNING,
	LS_ERROR, 
	INFO = LS_INFO,
	WARNING = LS_WARNING, 
	LERROR = LS_ERROR
};

namespace base{

	class StreamInterface;

	class LogMessage {
	public:
		LogMessage(const char *file,int line,LoggingSeverity sev);
		~LogMessage(void);

		std::ostream& stream() {
			return print_stream_;
		}

		//static 
		static void AddLogToStream(StreamInterface*);
		static void RemoveLogToStream(StreamInterface*);
		static void SetMinSev(LoggingSeverity sev);

	protected:
		//disable default constructor
		LogMessage(){};
		void OutputToDebug(const string&);
		void OutputToStream(StreamInterface *,const string&);
	private:
		std::ostringstream print_stream_;
		typedef std::vector<StreamInterface*> StreamList;
		static CriticalSection section_;
		static StreamList streams_;
		static LoggingSeverity min_sev_;

		LoggingSeverity sev_;
	};

#define LOG(sev)  \
	LogMessage(__FILE__,__LINE__,sev).stream()

} //namespace base


#endif
