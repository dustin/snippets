/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: cdda.c,v 1.1 1998/03/10 02:49:19 dustin Exp $
 */

#include <stdio.h>
#include <assert.h>
#include <sys/types.h>
#include <dmedia/cdaudio.h>

int main(int argc, char **argv)
{
    CDPLAYER *cd;
    CDSTATUS status;
    CDFRAME buf[12];
    CDTRACKINFO info;
    FILE *f;
    int i, n, pos, howfar;

    cd=CDopen(NULL, "r");
    if(cd==NULL)
    {
	perror("CDopen");
	exit(1);
    }

    /* DOWN WITH ROOT! */
    seteuid(getuid());
    setuid(getuid());

    if(CDgettrackinfo(cd, atoi(argv[1]), &info)==0)
    {
	printf("No such track, bitch.\n");
	exit(1);
    }

    howfar=CDmsftoframe(info.total_min,
			info.total_sec,
			info.total_frame);

    printf("Getting from %d for %d\n",
	     CDmsftoframe(info.start_min,
			  info.start_sec,
			  info.start_frame),
             howfar);

    f=fopen(argv[2], "wb");
    assert(f);

    CDgetstatus(cd, &status);
    while(status.state!=CD_READY)
    {
	sleep(5);
	CDgetstatus(cd, &status);
    }

    CDseektrack(cd, atoi(argv[1]));
    pos=0;

    for(;;)
    {
	n=CDreadda(cd, buf, 12);
	if(n<0)
	{
	    perror("CDreadda");
	    exit(1);
	}
	if(n==0)
	    break;

	for(i=0; i<12; i++)
	{
	    /* Break if we're at the end */
	    if(++pos>howfar)
		break;
	    fwrite(buf[i].audio, CDDA_DATASIZE, 1, f);
	}

	/* We're done if pos>howfar */
	if(pos>howfar)
	    break;

	printf("Did %d      \r", pos);
	fflush(stdout);
    }
    puts("");

    CDclose(cd);
    fclose(f);
    exit(0);
}
