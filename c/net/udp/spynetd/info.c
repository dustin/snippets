#include <stdio.h>
#include "protocol.h"

void main(void)
{
	printf("Packetsize is %d bytes\nCan carry %d users\n",
		INFOSIZE, NUSERS);
}
