#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>
#include <pthread.h>
#include <netinet/in.h>
#include <sys/time.h>
#include <sys/types.h>          /* See NOTES */
#include <sys/socket.h>
#include <sys/epoll.h>

#define EPOLL_MAX     1024
#define EPOLL_WAIT_TIMEOUT    1000
#define DEFUALT_PORT  50000

static int connects = 0;
static int in = 0;

static int epwfd = -1;

static int last_in = 0;
static void print_statistics()
{
  fprintf(stderr, "current connects are %d ,in(%-12d Bytes/%-11d Bytes)\r", connects, in - last_in, in);
  last_in = in;
}

void release_event(struct epoll_event *event)
{
  int fd = event->data.fd;
  epoll_ctl(epwfd, EPOLL_CTL_DEL, fd, event);
  close(fd);
}

static void discard(struct epoll_event *event)
{
  int fd = event->data.fd;
  char buffer[4096] = {0};
  int readed;
  if(event->events & EPOLLIN == EPOLLIN) {
    while(1) {
      readed = read(fd, buffer, sizeof(buffer));
      if(readed == -1) {
        //由于socket是非阻塞的，当errno为EAGAIN时，表示缓冲区中
        //没有数据可读了
        if(errno == EAGAIN) {
          break;
        } else {
          connects --;
          release_event(event);
          return;
        }
      } else if(readed == 0) {
        //表明对端已经关闭
        connects --;
        release_event(event);
        break;
      } else {
        assert(readed <= sizeof(buffer));
        //write(fd, buffer, readed);
        in += readed;
      }
    }
  } else if (event->events & EPOLLRDHUP == EPOLLRDHUP) {
    printf("hup \n");
    connects --;
    release_event(event);
  } else if (event->events & EPOLLOUT == EPOLLOUT) {
    printf("out \n");
  } else if (event->events & EPOLLERR == EPOLLERR) {
    printf("err \n");
    connects --;
    release_event(event);
  }

}

static void echo(struct epoll_event *event) 
{
  int fd = event->data.fd;
  char buffer[4096] = {0};
  int readed;
  if(event->events & EPOLLIN == EPOLLIN) {
    while(1) {
      readed = read(fd, buffer, sizeof(buffer));
      if(readed == -1) {
        //由于socket是非阻塞的，当errno为EAGAIN时，表示缓冲区中
        //没有数据可读了
        if(errno == EAGAIN) {
          break;
        } else {
          connects --;
          release_event(event);
          return;
        }
      } else if(readed == 0) {
        //表明对端已经关闭
        connects --;
        release_event(event);
        break;
      } else {
        assert(readed <= sizeof(buffer));
        in += readed;
        write(fd, buffer, readed);
      }
    }
  } else if (event->events & EPOLLRDHUP == EPOLLRDHUP) {
    printf("hup \n");
    connects --;
    release_event(event);
  } else if (event->events & EPOLLOUT == EPOLLOUT) {
    printf("out \n");
  } else if (event->events & EPOLLERR == EPOLLERR) {
    connects --;
    release_event(event);
  }
}

static void *worker(void *args)
{
  //int epwfd = *(int*)args;
  int waits = 0;
  int i;
  struct epoll_event ev[EPOLL_MAX] = {0};
  while(1) {
    waits = epoll_wait(epwfd, ev, EPOLL_MAX, -1);  
    if(waits == -1) {
      printf("worker: epoll_wait failed \n");
      break;
    }

    for(i = 0; i < waits; i++) {
      discard(&ev[i]);
      //echo(&ev[i]);
    }
    print_statistics();
  }
  return NULL;
}



static int set_noblock(int fd) 
{
  int flags = fcntl(fd, F_GETFL, 0);
  if(flags == -1) {
    printf("fcntl failed, errno is %d \n", errno);
    return -1;
  }

  if(fcntl(fd, F_SETFL, flags | O_NONBLOCK) == -1) {
    printf("set noblock, errno is %d \n", errno);
    return -1;
  }
  return 0;
}

int main(int argc, char** argv)
{
  int epfd = -1;
  int srvfd = -1;
  int clientfd = -1;
  int waits = -1;
  int reuse = 1;
  int i;
  pthread_t th;
  socklen_t addlen = sizeof(struct sockaddr);
  struct epoll_event event;
  struct epoll_event ev[1024] = {0};
  struct sockaddr_in server;
  struct sockaddr_in clientaddr;
  memset(&server, 0, sizeof(server));
  memset(&clientaddr, 0, sizeof(clientaddr));
  memset(&event, 0, sizeof(event));

  srvfd = socket(AF_INET, SOCK_STREAM, 0);
  if(srvfd == -1) {
    printf("socket failed, errno is %d \n", errno);
    exit(-2);
  }

  if(setsockopt(srvfd, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(int)) == -1) {
    printf("reuse sockaddr failed, error is %s \n", strerror(errno));
  }

  //创建工作线程
  epwfd = epoll_create(sizeof(int));
  if(epwfd == -1) {
    printf("epoll_create failed, errno is %d \n", errno);
    exit(-1);
  }
  
  if(pthread_create(&th, NULL, worker, NULL)  != 0) {
    printf("create worker thread failed \n");
    exit(-1);
  }

  //处理服务socket
  if(set_noblock(srvfd) == -1) {
    exit(-1);
  }

  epfd = epoll_create(sizeof(int));
  if(epfd == -1) {
    printf("epoll_create failed, errno is %d \n", errno);
    exit(-1);
  }

  event.events = EPOLLIN;
  event.data.fd = srvfd;
  if(epoll_ctl(epfd, EPOLL_CTL_ADD, srvfd, &event) == -1) {
    printf("epoll_ctl failed, errno is %d \n", errno);
    exit(-1);
  }

  server.sin_family = AF_INET;
  server.sin_addr.s_addr = htonl(INADDR_ANY);
  /*
  if(inet_pton(AF_INET, "0.0.0.0", &server.sin_addr) == -1) {
    printf("inet_pton failed \n");
    exit(-1);
  }
  */
  server.sin_port = htons(DEFUALT_PORT);
  if(bind(srvfd, (struct sockaddr*)&server, sizeof(struct sockaddr)) == -1) {
    printf("bind failed, errno is %s \n",strerror(errno));
    exit(-1);
  }
  listen(srvfd, 5);
  printf("listen %d ..... \n", DEFUALT_PORT);

  while(1) {
    waits = epoll_wait(epfd, ev, EPOLL_MAX, EPOLL_WAIT_TIMEOUT);  
    if(waits == -1) {
      printf("epoll_wait failed, errno is %d \n", errno);
      exit(-1);
    } else if (waits == 0) {
      //timeout
      //time += EPOLL_WAIT_TIMEOUT;
      //float statistics = (float)(in / time);
      //fprintf(stderr, "current connects are %d ,in(%-11d Bytes/%-11d Bytes)\r", connects, in - last_in, in);
      //last_in = in;
      print_statistics();
      continue;
    }

    for(i = 0; i < waits; i++) {
      struct epoll_event clientev;
      //处理accept
      if(ev[i].data.fd == srvfd) {
        clientfd = accept(srvfd, (struct sockaddr*)&clientaddr, &addlen);
        if(clientfd == -1) {
          printf("accept failed, errno is %d \n", errno);
          exit(-1);
        }

        connects ++;

        set_noblock(clientfd);
        
        memset(&clientev, 0, sizeof(struct epoll_event));
        clientev.events = EPOLLIN|EPOLLOUT|EPOLLRDHUP|EPOLLET;
        clientev.data.fd = clientfd;
        //clientev->data.ptr = clientev;
        if(epoll_ctl(epwfd, EPOLL_CTL_ADD, clientfd, &clientev) == -1) {
          printf("epoll_ctl failed, errno is %s \n", strerror(errno));
          exit(-1);
        }
      } else {
        //处理客户端数据
        //echo(&ev[i]);
        //discard(&ev[i]);
        //已经交给独立的线程处理了
      }
    }
  }

  close(srvfd);
  close(epfd);
  close(epwfd);
  return 0;
}
