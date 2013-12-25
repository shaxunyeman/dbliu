#ifndef __H_EYOU_DANDELION_STREAM_H__
#define __H_EYOU_DANDELION_STREAM_H__

namespace base {

	class StreamInterface {
	public:
		StreamInterface(){

		}

		virtual ~StreamInterface(void){

		}

		virtual int write(const char *data,unsigned int size) = 0;
		virtual int read(char *buffer,unsigned int size) = 0;
	protected:
	private:
	};

} //namespace base

#endif  //__H_EYOU_DANDELION_STREAM_H__
