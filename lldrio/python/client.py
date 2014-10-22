import threading
import socket
import time
import sys
import string

g_host = "123.57.39.114"
g_port = 50000
g_threads = 4
g_process = 4
g_count = 10

data = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

argc = len(sys.argv)


for i in range(1, argc, 2):
  if(sys.argv[i] == "-h"):
    g_host = sys.argv[i + 1]
  elif (sys.argv[i] == "-p"):
    g_port = string.atoi(sys.argv[i + 1], 10)
  elif(sys.argv[i] == "-t"):
    g_threads = string.atoi(sys.argv[i + 1], 10)
  elif(sys.argv[i] == "-f"):
    g_process = string.atoi(sys.argv[i + 1], 10)
  elif(sys.argv[i] == "-c"):
    g_count = string.atoi(sys.argv[i + 1], 10)


class client(threading.Thread):
  def __init__(self):
    threading.Thread.__init__(self)
    self.host = g_host
    self.port = g_port 
    self.count = 0;

  def run(self):
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
      ret = sock.connect((self.host, self.port))
      while(True):
#time.sleep(1)
        sock.send(data)
        time.sleep(0.1)
        if(self.count > g_count):
          sock.close()
          return
        else:
          self.count += 1
    except socket.error,e:
      print "error = %s " %e
    except socket.timeout,e:
      print "timeout "


def test():
  cl_list = []
  for i in range(0,g_threads):
    cl = client()
    cl.start()
    cl_list.append(cl)

  for el in cl_list:
      el.join()

if __name__ == '__main__' :
  test()
