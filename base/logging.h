#ifndef __EYOU_DANDELION_LOGGING_H__
#define __EYOU_DANDELION_LOGGING_H__

#include <sstream>
#include <string>
#include <vector>

<<<<<<< HEAD
#include "criticalsection.h"
=======
#include "base/criticalsection.h"
>>>>>>> fe21f097603403f2b3eb7c02fe7cae4c50193754

using namespace std;


enum LoggingSeverity {
<<<<<<< HEAD
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

	  void OutputToDebug(const string&);
      void OutputToStream(StreamInterface *,const string&);

	  //static 
	  static void AddLogToStream(StreamInterface*);
	  static void RemoveLogToStream(StreamInterface*);
	  static void SetMinSev(LoggingSeverity sev);

    protected:
      //disable default constructor
      LogMessage(){};
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
=======
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

#define LAST_SYSTEM_ERROR \
	(::GetLastError())
>>>>>>> fe21f097603403f2b3eb7c02fe7cae4c50193754

} //namespace base


#endif
