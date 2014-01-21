<<<<<<< HEAD
#include "logfilestream.h"
=======
#include "base/logfilestream.h"
>>>>>>> fe21f097603403f2b3eb7c02fe7cae4c50193754

#define LOG_MAX_SIZE	10485760	//10M Bytes
#define TRUNC_OFFSET	7340032		//7M Bytes

namespace base {

	LogFileStream::LogFileStream(const char *logfile)
<<<<<<< HEAD
	:logfile_(logfile) {
		
=======
		:logfile_(logfile) {

>>>>>>> fe21f097603403f2b3eb7c02fe7cae4c50193754
	}

	LogFileStream::~LogFileStream(void) {
	}

	//StreamInterface
	int LogFileStream::write(const char *data,unsigned int size) {
		long logsize = tell_size();
		if (logsize > LOG_MAX_SIZE) 
			clip(logsize);

		ofstream ofs;
		ofs.open(logfile_.c_str(),ofstream::out | ofstream::app);
		if (!ofs.is_open())
			return -1;

		ofs << data;
		ofs.close();

		return 0;
	}

	int LogFileStream::read(char *buffer,unsigned int size) {
		return 0;
	}

	long LogFileStream::tell_size() {
		long size = 0;
		ifstream ifile;
		ifile.open(logfile_.c_str(),ios::binary);
		ifile.seekg(0,ios::end);
		size = ifile.tellg();
		ifile.close();
		return size;
	}

	int LogFileStream::clip(long logsize) {
		char *buffer = new char[logsize + 1];
		memset(buffer,0,logsize + 1);

		ifstream ifile;
		ifile.open(logfile_.c_str(), ios::binary);
		ifile.read(buffer,logsize);
		ifile.close();

		long offset = TRUNC_OFFSET;
		char rn = buffer[offset];
		while(rn != '\n') {
			offset ++;
			rn = buffer[offset];
		}
		string data(buffer + offset + 1);

		ofstream ofs;
		ofs.open(logfile_.c_str(),ofstream::trunc | ofstream::out);
		ofs << data.c_str();
		ofs.close();

		delete []buffer;
		return 0;
	}

} //namespace base