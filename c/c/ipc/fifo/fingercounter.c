#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>
#include <signal.h>

void main(void)
{
  FILE *fifo;
  int count=0;

  signal(SIGPIPE, SIG_IGN);

  while(1)
    {
      fifo=fopen("/usr/people/dustin/.plan","w");
      if((!fifo))
        exit(1);
      fprintf(fifo, "You are caller number %d\n", ++count);
      fclose(fifo);
      sleep(1);
    }
}
