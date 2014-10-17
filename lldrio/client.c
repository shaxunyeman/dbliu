#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <sys/types.h>          /* See NOTES */
#include <sys/socket.h>
#include <sys/epoll.h>

#define MAXLINE 4096

int main(int argc, char **argv)
{
  int    sockfd, n;
  char    recvline[4096], sendline[4096];
  struct sockaddr_in    servaddr;

  if( argc != 2){
    printf("usage: ./client <ipaddress>\n");
    exit(0);
  }

  if( (sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0){
    printf("create socket error: %s(errno: %d)\n", strerror(errno),errno);
    exit(0);
  }

  memset(&servaddr, 0, sizeof(servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_port = htons(8001);
  if( inet_pton(AF_INET, argv[1], &servaddr.sin_addr) <= 0){
    printf("inet_pton error for %s\n",argv[1]);
    exit(0);
  }

  if( connect(sockfd, (struct sockaddr*)&servaddr, sizeof(servaddr)) < 0){
    printf("connect error: %s(errno: %d)\n",strerror(errno),errno);
    exit(0);
  }

  printf("send msg to server: \n");
  while(1) {
    fgets(sendline, 4096, stdin);
    if(send(sockfd, sendline, strlen(sendline), 0) < 0)
    {
      printf("send msg error: %s(errno: %d)\n", strerror(errno), errno);
      exit(0);
    }

    if(strncasecmp(sendline, "quit", 4) == 0 || strncasecmp(sendline, "q", 1) == 1) {
      break;
    }

    if(recv(sockfd, recvline, sizeof(recvline), 0) < 0) {
      printf("recv failed \n");
      exit(0);
    } else {
      printf("%s \n", recvline);
    }
  }
  close(sockfd);
  exit(0);
}
