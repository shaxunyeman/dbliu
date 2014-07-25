#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <arpa/inet.h>
#include <sys/epoll.h>
#include <sys/types.h>
#include <sys/socket.h>

#define MAX_CONNECTS  1024

int setsocketnonblocking(int fd)
{
  int flags;
  flags = fcntl(fd,F_GETFL,0);
  if(flags == -1) {
    printf("%s,fcntl failed \n",strerror(errno));
    return -1;
  }

  if(fcntl(fd,F_SETFL,flags|O_NONBLOCK) == -1) {
    return -1;
  }

  return 0;
}

int
main(int argc,char **argv)
{
  int connects = 0;
  int listenfd = -1;
  int clientfd = -1;
  int epfd = -1;
  int waits = 0;
  int readed = -1;
  int closed = 0;
  int exception = 0;
  int i;
  int reused = 1;
  int sndbuf = 1024;
  char buffer[1025] = {0};
  char sndbuffer[1024] = {'a'};
  struct epoll_event ev,*events;
  struct sockaddr_in serveraddr;
  struct sockaddr_in clientaddr;
  socklen_t socklen = sizeof(struct sockaddr_in);

  memset(&serveraddr,0,socklen);
  serveraddr.sin_family = AF_INET;
  serveraddr.sin_port = htons(9001);
  serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);

  if((epfd = epoll_create(MAX_CONNECTS)) == -1) {
    printf("%s,epoll_create faild \n",strerror(errno));
    exit(-1);
  }
  listenfd = socket(AF_INET,SOCK_STREAM,0);
  if(listen < 0) {
    printf("%s,socket failed \n",strerror(listenfd));
    exit(-1);
  }

  setsockopt(listenfd,SOL_SOCKET,SO_REUSEADDR,&reused,sizeof(int));

  if(setsocketnonblocking(listenfd) == -1) {
    printf("%s,setnonblock faild \n",strerror(errno));
    exit(-1);
  }

  ev.data.fd = listenfd;
  ev.events = EPOLLIN|EPOLLET;
  if(epoll_ctl(epfd,EPOLL_CTL_ADD,listenfd,&ev) == -1) {
    printf("%s,epoll_ctl faild \n",strerror(errno));
    exit(-1);
  }

  if(bind(listenfd,(const struct sockaddr*)&serveraddr,socklen) == -1) {
    printf("%s,bind faild \n",strerror(errno));
    exit(-1);
  }

  if (listen(listenfd,5) == -1) {
    printf("%s,listen faild \n",strerror(errno));
    exit(-1);
  }

  events = (struct epoll_event*)malloc(sizeof(struct epoll_event)*MAX_CONNECTS);
  if(events == NULL) {
    printf("%s,malloc faild \n",strerror(errno));
    exit(-1);
  }

  printf("EPOLLIN = %d\nEPOLLOUT=%d\nEPOLLERR=%d\nEPOLLET=%d\n",EPOLLIN,EPOLLOUT,EPOLLERR,EPOLLET);
    
  printf("listen 9001 %d .... \n",listenfd);
  while(true) {
    waits = epoll_wait(epfd,events,MAX_CONNECTS,-1);
    if(waits == -1) {
      printf("%s,epoll_wait failed \n",strerror(errno));
      exit(-1);
    }
    
    if(waits == 0)
      continue;

    for(i = 0; i < waits; i++) {
      if(events[i].data.fd == listenfd) { 
          if(events[i].events & EPOLLIN == EPOLLIN) {
            clientfd = accept(listenfd,(struct sockaddr*)&clientaddr,&socklen);           
            if(clientfd == -1) {
              printf("%s,accept failed \n",strerror(errno));
              exit(-1);
            }
            setsocketnonblocking(clientfd);
            setsockopt(clientfd,SOL_SOCKET,SO_SNDBUF,&sndbuf,sizeof(int));
            connects++;
            printf("connects currently: %d \n",connects);
            ev.data.fd = clientfd;
            ev.events = EPOLLIN|EPOLLOUT|EPOLLET;
            if(epoll_ctl(epfd,EPOLL_CTL_ADD,clientfd,&ev) == -1) {
              printf("%s,epoll_ctl faild \n",strerror(errno));
              exit(-1);
            }
            
          } else if(events[i].events & EPOLLIN == EPOLLERR) {
            printf("listen sock error,%s \n",strerror(errno));
            exit(-1);
          }
      } else {
        clientfd = events[i].data.fd;
        if(events[i].events & EPOLLIN == EPOLLIN) {
          while(true) {
            readed = read(clientfd,buffer,sizeof(buffer) - 1);
            if(readed < 0) {
              if(errno == EAGAIN) {
                break;
              }
              else {
                connects --;
                exception ++;
                printf("connects currently: %d (n:%d,e:%d)\n",connects,closed,exception);

                ev.data.fd = clientfd;
                ev.events = EPOLLIN|EPOLLOUT|EPOLLET;
                if(epoll_ctl(epfd,EPOLL_CTL_DEL,clientfd,&ev) == -1) {
                  printf("%s,epoll_ctl faild \n",strerror(errno));
                  exit(-1);
                }

                close(clientfd);
              }

            } else if (readed == 0) {
              connects --;
              closed ++;
              printf("connects currently: %d (n:%d,e:%d)\n",connects,closed,exception);
              
              ev.data.fd = clientfd;
              ev.events = EPOLLIN|EPOLLOUT|EPOLLET;
              if(epoll_ctl(epfd,EPOLL_CTL_DEL,clientfd,&ev) == -1) {
                printf("%s,epoll_ctl faild \n",strerror(errno));
                exit(-1);
              }
              
              close(clientfd);
              break;
            } else {
              ;
            }
          }

        } else if(events[i].events & EPOLLOUT == EPOLLOUT) {
          //printf("%d: out ... \n",time(NULL));
          int ret = write(clientfd,sndbuffer,sizeof(sndbuffer));
          if(ret < 0 ) {
            if(errno == EAGAIN) {
              int sndbuf = 0;
              int size = sizeof(int);
              getsockopt(clientfd,SOL_SOCKET,SO_SNDBUF,&sndbuf,&size);
              //printf("%d: write eagain %d\n",time(NULL),sndbuf);
              //sleep(4);
            }
          }
        }
        
      }
    }
  }

  if(events)
    free(events);

  close(epfd);
  close(listenfd);
  return 0;
}
