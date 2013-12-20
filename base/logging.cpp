#include <windows.h>

#include "logging.h"
#include "stream.h"

namespace base {

  const char* SeverityToString(LoggingSeverity sev) {
    if (sev == INFO)
      return "INFO";
    else if (sev == WARNING)
      return "WARNING";
    else if (sev == LERROR)
      return "ERROR";

    return "Unkown";

  }

  LoggingSeverity LogMessage::min_sev_ = WARNING;
  LogMessage::StreamList LogMessage::streams_;
  CriticalSection LogMessage::section_;

  LogMessage::LogMessage(const char *file,int line,LoggingSeverity sev)
  :sev_(sev) {
#ifdef _DEBUG
    print_stream_ << file << ":" << line;
#endif //_DEBUG
	print_stream_ << " [" << SeverityToString(sev) << "] ";
  }

  LogMessage::~LogMessage(void) {
	if (sev_ < min_sev_)
		return;

	print_stream_ << std::endl;
    string data = print_stream_.str();
	OutputToDebug(data);

	CritScope cri(&section_);
    StreamList::iterator it = streams_.begin();
    for(; it != streams_.end(); it++) {
      OutputToStream(*it,data);
    }
  }

  void LogMessage::OutputToDebug(const string& data) {
	  ::OutputDebugStringA(data.c_str());
	  HANDLE error_handle = ::GetStdHandle(STD_ERROR_HANDLE);
	  DWORD written = 0;
	  ::WriteFile(error_handle, data.data(), static_cast<DWORD>(data.size()), &written, 0);
  }

  void LogMessage::OutputToStream(StreamInterface* stream,const string& data){
    stream->write(data.c_str(),data.size());
  }

  void LogMessage::AddLogToStream(StreamInterface* pStream){
	  CritScope cri(&section_);
	  streams_.push_back(pStream);
  }

  void LogMessage::RemoveLogToStream(StreamInterface* pStream){
	  CritScope cri(&section_);
	  StreamList::iterator it = streams_.begin();
    for(; it != streams_.end(); it++) {
      if (*it == pStream) {
		  streams_.erase(it);
		  break;
	  }
    }
  }

  void LogMessage::SetMinSev(LoggingSeverity sev) {
	  min_sev_ = sev;
  }

}
