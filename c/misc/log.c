#include <stdio.h>
#include <syslog.h>

void main(void)
{
    int i;
    char s[80];

    openlog(NULL, 0, LOG_LOCAL0);

    for(i=0; ; i++)
    {
	sprintf(s, "Dustin can count to %d\n", i);
	syslog(LOG_DEBUG, s);
    }
}
