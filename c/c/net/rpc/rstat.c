/*
 * Copyright (c) 1998  Dustin Sallings
 *
 * $Id: rstat.c,v 1.4 1998/05/14 17:00:02 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <rpcsvc/rstat.h>

#ifndef FSCALE
#define FSHIFT  8
#define FSCALE  (1<<FSHIFT)
#endif

#ifndef CPUSTATES
# ifndef RSTAT_CPUSTATES
#  error "I don't know how to determine CPU states"
# endif
# define CPUSTATES RSTAT_CPUSTATES
# define DK_NDRIVE RSTAT_DK_NDRIVE
# define CP_USER   RSTAT_CPU_USER
# define CP_NICE   RSTAT_CPU_NICE
# define CP_SYS    RSTAT_CPU_SYS
# define CP_IDLE   RSTAT_CPU_IDLE
#endif

#define BIT(a)     (1<<a)

#define DO_CPU     BIT(0)
#define DO_DISK    BIT(1)
#define DO_PAGE    BIT(2)
#define DO_SWAP    BIT(3)
#define DO_INTR    BIT(4)
#define DO_NET     BIT(5)
#define DO_SWTCH   BIT(6)
#define DO_LOAD    BIT(7)
#define DO_BOOT    BIT(8)
#define DO_TIME    BIT(9)
#define DO_UPTIME  BIT(10)

#define DO_ALL (DO_CPU|DO_DISK|DO_PAGE|DO_SWAP|DO_INTR|DO_NET|DO_SWTCH| \
                DO_LOAD|DO_BOOT|DO_TIME|DO_UPTIME)

static void printuptime(int secs)
{
    int days, hours, minutes;
    days=hours=minutes=0;

    if(secs>60) {
	minutes=secs/60;
	secs=secs%60;
    }

    if(minutes>60) {
	hours=minutes/60;
	minutes=minutes%60;
    }

    if(hours>24) {
	days=hours/24;
	hours=hours%24;
    }

    if(days>0)
	printf("%d days ", days);
    if(hours>0)
	printf("%d hours ", hours);
    if(minutes>0)
	printf("%d minutes ", minutes);
    if(secs>0)
	printf("%d secs ", secs);

    puts("");
}

static void usage(char *cmd)
{
    printf("Usage:  %s [-acdpsinvlbtu] host ...\n"
           "-a print all statistics\n"
           "-c print CPU states\n"
           "-d print disk states\n"
           "-p print paging states\n"
           "-s print swapping states\n"
           "-i print interrupt states\n"
           "-n print network states\n"
           "-v print context switching states\n"
           "-l print load averages\n"
           "-b print boot time\n"
           "-t print current time\n"
           "-u print uptime\n", cmd);
}

static void printstatp(struct statstime statp[2], int flags)
{
    int i;
    char *cpustates[]={
	"user","nice","sys","idle"
    };

    if(flags&DO_CPU) {
        printf("CPU States:\n");
        for(i=0; i<CPUSTATES; i++)
	    printf("\t%s:\t%.2f\n", cpustates[i],
		           (float)(statp[1].cp_time[i]-statp[0].cp_time[i]));
        puts("");
    }

    if(flags&DO_DISK) {
        printf("Disk States:\n");
        for(i=0; i<DK_NDRIVE; i++)
	    printf("\t%d:  %.2f\n", i,
	           (float)(statp[1].dk_xfer[i]-statp[0].dk_xfer[i]));
        puts("");
    }

    if(flags&DO_PAGE) {
        printf("Page In:   %.2f\t\tOut:  %.2f\n",
               (float)(statp[1].v_pgpgin-statp[0].v_pgpgin),
               (float)(statp[1].v_pgpgout-statp[0].v_pgpgout));
    }
    if(flags&DO_SWAP) {
        printf("Swap In:   %.2f\t\tOut:  %.2f\n",
	       (float)(statp[1].v_pswpin-statp[0].v_pswpin),
               (float)(statp[1].v_pswpout-statp[0].v_pswpout));
    }
    if(flags&DO_INTR)
       printf("Intrpt:    %.2f\n\n", (float)(statp[1].v_intr-statp[0].v_intr));

    if(flags&DO_NET) {
        printf("In packets:   %.2f\tIn Errors:   %.2f\n",
	       (float)(statp[1].if_ipackets-statp[0].if_ipackets),
	       (float)(statp[1].if_ierrors-statp[0].if_ierrors));
        printf("Out packets:  %.2f\tOut Errors:  %.2f\n",
	       (float)(statp[1].if_opackets-statp[0].if_opackets),
	       (float)(statp[1].if_oerrors-statp[0].if_oerrors));
        printf("Collisions:   %.2f\n",
	       (float)(statp[1].if_collisions-statp[0].if_collisions));
        puts("");
    }

    if(flags&DO_SWTCH)
        printf("vswtch:  %.2f\n", (float)(statp[1].v_swtch-statp[0].v_swtch));

    if(flags&DO_LOAD)
        printf("Loads:  %.2f, %.2f, %.2f\n",
	    (double)statp[1].avenrun[0]/FSCALE,
	    (double)statp[1].avenrun[1]/FSCALE,
	    (double)statp[1].avenrun[2]/FSCALE);

    if(flags&DO_BOOT)
        printf("Boottime:  %s", ctime(&statp[1].boottime.tv_sec));
    if(flags&DO_TIME)
        printf("Current:   %s", ctime(&statp[1].curtime.tv_sec));
    if(flags&DO_UPTIME) {
        printf("Uptime:    ");
        printuptime(statp[1].curtime.tv_sec-statp[1].boottime.tv_sec);
    }
}

static void dohost(char *host, int flags)
{
    struct statstime statp[2];
    int r=0;
    r+=rstat(host, &statp[0]);
    if(flags&(DO_CPU|DO_DISK|DO_PAGE|DO_SWAP|DO_INTR|DO_NET|DO_SWTCH)) {
        sleep(1);
        r+=rstat(host, &statp[1]);
    } else {
	statp[1]=statp[0];
    }

    if(r == 0) {
        printf("%s:\n", host);
	printstatp(statp, flags);
    } else {
       printf("rstat of %s was unsuccessful\n", host);
    }
}

int main(int argc, char **argv)
{
    int flags=0, c;
    extern int optind;

    if(argc < 2) {
	usage(argv[0]);
	exit(1);
    }

    while( (c=getopt(argc, argv, "acdpsinvlbtu")) != -1) {
	switch(c) {
	    case 'a': flags|=DO_ALL; break;
	    case 'c': flags|=DO_CPU; break;
	    case 'd': flags|=DO_DISK; break;
	    case 'p': flags|=DO_PAGE; break;
	    case 's': flags|=DO_SWAP; break;
	    case 'i': flags|=DO_INTR; break;
	    case 'n': flags|=DO_NET; break;
	    case 'v': flags|=DO_SWTCH; break;
	    case 'l': flags|=DO_LOAD; break;
	    case 'b': flags|=DO_BOOT; break;
	    case 't': flags|=DO_TIME; break;
	    case 'u': flags|=DO_UPTIME; break;
	    case '?': usage(argv[0]); exit(1);
	}
    }

    while(optind<argc) {
	dohost(argv[optind++], flags);
    }

    return(0);
}
