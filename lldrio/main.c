#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>
#include <netinet/in.h>
#include <sys/types.h>          /* See NOTES */
#include <sys/socket.h>
#include <sys/epoll.h>

#define EPOLL_MAX 1024

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
          return;
        }
      } else if(readed == 0) {
        //表明对端已经关闭
        printf("read 0 (%d)\n", fd);
        close(fd);
        //free(event->data.ptr);
        free(event);
        break;
      } else {
        assert(readed <= sizeof(buffer));
        write(fd, buffer, readed);
      }
    }
  } else if (event->events & EPOLLRDHUP == EPOLLRDHUP) {
    printf("shut down %d \n", fd);
    close(fd);
    //free(event->data.ptr);
    free(event);
  } else if (event->events & EPOLLOUT == EPOLLOUT) {
    printf("out \n");
  }
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
  size_t addlen = sizeof(struct sockaddr);
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
  server.sin_port = htons(8001);
  if(bind(srvfd, (struct sockaddr*)&server, sizeof(struct sockaddr)) == -1) {
    printf("bind failed, errno is %s \n",strerror(errno));
    exit(-1);
  }
  listen(srvfd, 5);
  printf("listen %d ..... \n", srvfd);
  
  while(1) {
    waits = epoll_wait(epfd, ev, EPOLL_MAX, -1);  
    if(waits == -1) {
      printf("epoll_wait failed, errno is %d \n", errno);
      exit(-1);
    }

    for(i = 0; i < waits; i++) {
      struct epoll_event *clientev = NULL;
      printf("wait (%d/%d) %d \n",i, waits, ev[i].data.fd);
      //处理accept
      if(ev[i].data.fd == srvfd) {
        clientfd = accept(srvfd, (struct sockaddr*)&clientaddr, &addlen);
        if(clientfd == -1) {
          printf("accept failed, errno is %d \n", errno);
          exit(-1);
        }
        printf("coming client %d \n", clientfd);
        set_noblock(clientfd);
        
        clientev = (struct epoll_event*)malloc(sizeof(struct epoll_event));
        if(clientev == NULL) {
          printf("memory out \n");
          exit(-1);
        }
        memset(clientev, 0, sizeof(struct epoll_event));
        clientev->events = EPOLLIN|EPOLLOUT|EPOLLRDHUP|EPOLLET;
        clientev->data.fd = clientfd;
        //clientev->data.ptr = clientev;
        if(epoll_ctl(epfd, EPOLL_CTL_ADD, clientfd, clientev) == -1) {
          printf("epoll_ctl failed, errno is %s \n", strerror(errno));
          exit(-1);
        }
      } else {
        //处理客户端数据
        echo(&ev[i]);
      }
    }
  }

  close(srvfd);
  close(epfd);
  return 0;
}
