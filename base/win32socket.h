#ifndef __H_EYOU_DANDELION_WIN32SOCKET_H__
#define __H_EYOU_DANDELION_WIN32SOCKET_H__

#include "base/asyncsocket.h"
#include "base/socketaddress.h"
#include "base/socketserver.h"
#include "base/criticalsection.h"
#include "base/messagequeue.h"
#include "base/win32window.h"

namespace base {

	class Win32Socket : public AsyncSocket {
	public:
		Win32Socket();
		~Win32Socket();

		bool CreateT(int family,int type);
		int Attach(SOCKET s);
		void SetTimeout(int ms);

		// AsyncSocket Interface
		virtual SocketAddress GetLocalAddress() const;
		virtual SocketAddress GetRemoteAddress() const;
		virtual int Bind(const SocketAddress& addr);
		virtual int Connect(const SocketAddress& addr);
		virtual int Send(const void *pv,size_t cb);
		virtual int SendTo(const void *pv, size_t cb, const SocketAddress& addr);
		virtual int Recv(void *pv,size_t cb);
		virtual int RecvFrom(void *pv, size_t cb, SocketAddress *paddr);
		virtual int Listen(int backlog);
		virtual Win32Socket *Accept(SocketAddress *out_addr);
		virtual int Close();
		virtual int GetError() const;
		virtual void SetError(int error);
		virtual ConnState GetState() const;
		virtual int EstimateMTU(unsigned short* mtu);
		virtual int GetOption(Option opt, int* value);
		virtual int SetOption(Option opt, int value);
	protected:
	private:
		void OnSocketNotify(SOCKET socket, int event, int error);
		void OnDnsNotify(HANDLE task, int error);
		bool HandleClosed(int close_error);
		int DoConnect(const SocketAddress& addr);
		bool SetAsync(int events);
		void Win32Socket::CreateSink();
		void UpdateLastError();
		void PostClosed();
		static int TranslateOption(Option opt, int* slevel, int* sopt);

		SOCKET socket_;
		int error_;
		ConnState state_;
		SocketAddress addr_;         // address that we connected to (see DoConnect)
		bool closing_;
		int close_error_;

		class EventSink;
		friend class EventSink;
		EventSink * sink_;

		struct DnsLookup;
		DnsLookup * dns_;
	};

	class Win32SocketServer : public SocketServer {
	public:
		Win32SocketServer(MessageQueue* message_queue);
		~Win32SocketServer();

		void set_modeless_dialog(HWND hdlg) {
			hdlg_ = hdlg;
		}

		//from socketfactory
		virtual Socket* CreateSocket(int type);
		virtual Socket* CreateSocket(int family,int type);
		virtual AsyncSocket* CreateAsyncSocket(int type);
		virtual AsyncSocket* CreateAsyncSocket(int family,int type);

		//socketserver
		virtual void SetMessageQueue(MessageQueue* queue);
		virtual bool Wait(int cms,bool process_io);
		virtual void WakeUp();

		void Pump();

		HWND handle() {
			return wnd_.handle();
		}

	protected:
	private:
		class MessageWindow : public Win32Window {
		public:
			explicit MessageWindow(Win32SocketServer* ss) 
			:ss_(ss) {
			}
		private:
			virtual bool OnMessage(UINT uMsg, WPARAM wParam, LPARAM lParam,LRESULT& result);
			Win32SocketServer* ss_;
		};

		static const TCHAR kWindowName[];
		MessageQueue *message_queue_;
		MessageWindow wnd_;
		CriticalSection cs_;
		bool posted_;
		HWND hdlg_;
	};

}	//namespace base

#endif	//__H_EYOU_DANDELION_WIN32SOCKET_H__