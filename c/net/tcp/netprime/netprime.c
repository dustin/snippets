#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/select.h>

#include "netprime.h"

struct listthing commandlist[]={
    { "quit", quit },
    { "generate", generate },
    { NULL, NULL }
};

void quit(int s, char *arg)
{
    exit(0);
}

void generate(int s, char *arg)
{
    int i, from;
    char out[20];

    if(strlen(arg)>0)
        from=atoi(arg);
    else
        from=2;

    for(i=from; i<from+NGEN; i++)
    {
        sprintf(out, "%d\n", i);
        write(s, out, strlen(out));
        sleep(1);
    }
}

void parsecommand(int s, char *command)
{
    int i, ol;
    char *cmd, *arg;

    cmd=command;

    ol=strlen(command);

    for(i=0; ; i++)
    {
        if(cmd[i]==' ' || cmd[i]==0x00)
        {
            cmd[i]=0x00;
            break;
        }
    }

    if(i==ol)
        arg="";
    else
        arg=cmd+i+1;

    for(i=0; commandlist[i].name!=NULL; i++)
    {
        if(strcmp(commandlist[i].name, cmd)==0)
        {
            break;
        }
    }

    if(commandlist[i].name != NULL)
    {
        commandlist[i].func(s, arg);
    }
}

void reaper(void)
{
    int pid=1;
    while(pid>0)
        pid=waitpid(0, NULL, WUNTRACED | WNOHANG);
}

void main(void)
{
    int listen, s, i, fromlen, pid;
    struct sockaddr_in fsin;
    char buf[BUFLEN];
    fd_set fdset, tfdset;
    struct timeval t;

    listen=getservsocket(PRIME_PORT);

    FD_ZERO(&tfdset);
    FD_SET(listen, &tfdset);

    for(;;)
    {
        fdset=tfdset;
        t.tv_sec=180;
        t.tv_usec=0;
        fromlen=sizeof(fsin);

        if(select(listen+1, &fdset, NULL, NULL, &t) >0)
        {
            s=accept(listen, (struct sockaddr *)&fsin, &fromlen);

            if( (pid=fork()) > 0)
            {
                close(s);
            }
            else
            {
                for(;;)
                {
                    i=gettextcr(s, buf);
                    parsecommand(s, buf);
                } /* parse for */
            } /* fork if */
        } /* select if */
	reaper();
    } /* main for */
}
