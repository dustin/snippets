#include <stdio.h>

#include <mbkd.h>

#define AUTHDATA "630712e3e78e9ac261f13b8918c1dbdc"

void
main(void)
{
	int     i;
	char    buf1[80], buf2[80];
	MBK    *mbk;

	mbk = mbk_new("localhost", 1099, AUTHDATA);
	for (i = 0; i < 5; i++) {
		sprintf(buf1, "thing%d", i);
		sprintf(buf2, "data%d", i);
		if (mbk->append(mbk, buf1, buf2) < 0)
			break;
	}

	mbk->send(mbk);
	mbk->destroy(mbk);
}
