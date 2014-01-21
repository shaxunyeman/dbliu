#ifndef __H_BASE_PHYSICALSOCKETSERVER_H__
#define __H_BASE_PHYSICALSOCKETSERVER_H__

#include <vector>

#include "base/scoped_ptr.h"
#include "base/socketserver.h"
#include "base/criticalsection.h"

namespace base {

// Event constants for the Dispatcher class.
enum DispatcherEvent {
  DE_READ    = 0x0001,
  DE_WRITE   = 0x0002,
  DE_CONNECT = 0x0004,
  DE_CLOSE   = 0x0008,
  DE_ACCEPT  = 0x0010,
};

class Signaler;

class Dispatcher {
 public:
  virtual ~Dispatcher() {}
  virtual uint32 GetRequestedEvents() = 0;
  virtual void OnPreEvent(uint32 ff) = 0;
  virtual void OnEvent(uint32 ff, int err) = 0;
  virtual WSAEVENT GetWSAEvent() = 0;
  virtual SOCKET GetSocket() = 0;
  virtual bool CheckSignalClose() = 0;
};

// A socket server that provides the real sockets of the underlying OS.
class PhysicalSocketServer : public SocketServer {
 public:
  PhysicalSocketServer();
  virtual ~PhysicalSocketServer();

  // SocketFactory:
  virtual Socket* CreateSocket(int type);
  virtual Socket* CreateSocket(int family, int type);

  virtual AsyncSocket* CreateAsyncSocket(int type);
  virtual AsyncSocket* CreateAsyncSocket(int family, int type);

  // Internal Factory for Accept
  AsyncSocket* WrapSocket(SOCKET s);

  // SocketServer:
  virtual bool Wait(int cms, bool process_io);
  virtual void WakeUp();

  void Add(Dispatcher* dispatcher);
  void Remove(Dispatcher* dispatcher);

 private:
  typedef std::vector<Dispatcher*> DispatcherList;
  typedef std::vector<size_t*> IteratorList;

  DispatcherList dispatchers_;
  IteratorList iterators_;
  Signaler* signal_wakeup_;
  CriticalSection crit_;
  bool fWait_;
  uint32 last_tick_tracked_;
  int last_tick_dispatch_count_;
  WSAEVENT socket_ev_;
};

} // namespace base

#endif // __H_BASE_PHYSICALSOCKETSERVER_H__
