/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: cdda.c,v 1.3 1998/09/20 02:28:24 dustin Exp $
 */

#include <stdio.h>
#include <assert.h>
#include <sys/types.h>
#include <dmedia/cdaudio.h>
#include <dmedia/audiofile.h>

int main(int argc, char **argv)
{
    CDPLAYER *cd;
    CDSTATUS status;
    CDFRAME buf[12];
    CDTRACKINFO info;

    FILE *f;
    int i, n, pos, howfar;

    /* Yeah, I know this is nasty, but it worksish */
    unsigned char wavheader[]={
        0x52, 0x49, 0x46, 0x46, 0x24, 0x40, 0x17, 0x00, 0x57, 0x41, 0x56,
	0x45, 0x66, 0x6d, 0x74, 0x20, 0x10, 0x00, 0x00, 0x00, 0x01, 0x00,
	0x02, 0x00, 0x44, 0xac, 0x00, 0x00, 0x10, 0xb1, 0x02, 0x00, 0x04,
	0x00, 0x10, 0x00, 0x64, 0x61, 0x74, 0x61, 0x00, 0x40, 0x17, 0x00,
    };

    cd=CDopen(NULL, "r");
    if(cd==NULL) {
	perror("CDopen");
	exit(1);
    }

    /* DOWN WITH ROOT! */
    seteuid(getuid());
    setuid(getuid());

    if(CDgettrackinfo(cd, atoi(argv[1]), &info)==0) {
	fprintf(stderr, "No such track, bitch.\n");
	exit(1);
    }

    howfar=CDmsftoframe(info.total_min,
			info.total_sec,
			info.total_frame);

    fprintf(stderr, "Getting from %d for %d\n",
	     CDmsftoframe(info.start_min,
			  info.start_sec,
			  info.start_frame),
             howfar);

    /* File or stdout? */
    if(strcmp(argv[2], "-")==0) {
        f=stdout;
    } else {
        f=fopen(argv[2], "wb");
    }
    assert(f);

    /* Nasty hack for file header */
    fwrite(wavheader, sizeof(wavheader), 1, f);

    CDgetstatus(cd, &status);
    while(status.state!=CD_READY) {
	sleep(5);
	CDgetstatus(cd, &status);
    }

    CDseektrack(cd, atoi(argv[1]));
    pos=0;

    for(;;) {
	n=CDreadda(cd, buf, 12);
	if(n<0) {
	    perror("CDreadda");
	    exit(1);
	}
	if(n==0)
	    break;

	for(i=0; i<12; i++) {
	    /* Break if we're at the end */
	    if(++pos>howfar)
		break;
	    fwrite(buf[i].audio, CDDA_DATASIZE, 1, f);
	}

	/* We're done if pos>howfar */
	if(pos>howfar)
	    break;

	fprintf(stderr, " Did %d      \r", pos);
	fflush(stderr);
    }
    puts("");

    CDclose(cd);
    close(f);
    exit(0);
}
