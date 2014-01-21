#ifndef __H_EYOU_DANDELION_STREAM_H__
#define __H_EYOU_DANDELION_STREAM_H__

namespace base {

<<<<<<< HEAD
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
=======
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
>>>>>>> fe21f097603403f2b3eb7c02fe7cae4c50193754

} //namespace base

#endif  //__H_EYOU_DANDELION_STREAM_H__
