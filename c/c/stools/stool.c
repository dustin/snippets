#include <unistd.h>
#include <syslog.h>

void main(int argc, char **argv)
{
    syslog(LOG_NOTICE|LOG_AUTH, "S TOOL:  %s ran %s",
	    getlogin(), PATH);

    setuid(0);
    seteuid(0);

    argv[0]=PATH;
    execv(PATH, argv);
}
