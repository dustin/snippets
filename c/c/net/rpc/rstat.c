#include <stdio.h>
#include <rpcsvc/rstat.h>

#define FSHIFT  8
#define FSCALE  (1<<FSHIFT)

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

static void printstatp(struct statstime statp[2])
{
    int i;
    char *cpustates[]={
	"user","nice","sys","idle"
    };

    printf("CPU States:\n");
    for(i=0; i<CPUSTATES; i++)
	printf("\t%s:\t%.2f\n", cpustates[i],
		       (float)(statp[1].cp_time[i]-statp[0].cp_time[i]));

    puts("");

    printf("Disk States:\n");
    for(i=0; i<DK_NDRIVE; i++)
	printf("\t%d:  %.2f\n", i,

	(float)(statp[1].dk_xfer[i]-statp[0].dk_xfer[i]));

    puts("");

    printf("Page In:   %.2f\t\tOut:  %.2f\n",
		   (float)(statp[1].v_pgpgin-statp[0].v_pgpgin),
                   (float)(statp[1].v_pgpgout-statp[0].v_pgpgout));
    printf("Swap In:   %.2f\t\tOut:  %.2f\n",
		   (float)(statp[1].v_pswpin-statp[0].v_pswpin),
                   (float)(statp[1].v_pswpout-statp[0].v_pswpout));
    printf("Intrpt:    %.2f\n", (float)(statp[1].v_intr-statp[0].v_intr));

    puts("");

    printf("In packets:   %.2f\tIn Errors:   %.2f\n",
		       (float)(statp[1].if_ipackets-statp[0].if_ipackets),
		       (float)(statp[1].if_ierrors-statp[0].if_ierrors));
    printf("Out packets:  %.2f\tOut Errors:  %.2f\n",
		       (float)(statp[1].if_opackets-statp[0].if_opackets),
		       (float)(statp[1].if_oerrors-statp[0].if_oerrors));
    printf("Collisions:   %.2f\n",
		       (float)(statp[1].if_collisions-statp[0].if_collisions));

    puts("");

    printf("vswtch:  %.2f\n", (float)(statp[1].v_swtch-statp[0].v_swtch));

    printf("Loads:  %.2f, %.2f, %.2f\n",
	(double)statp[1].avenrun[0]/FSCALE,
	(double)statp[1].avenrun[1]/FSCALE,
	(double)statp[1].avenrun[2]/FSCALE);

    printf("Boottime:  %s", ctime(&statp[1].boottime.tv_sec));
    printf("Current:   %s", ctime(&statp[1].curtime.tv_sec));
    printf("Uptime:    ");
    printuptime(statp[1].curtime.tv_sec-statp[1].boottime.tv_sec);
}

int main(int argc, char **argv)
{
    struct statstime statp[2];
    int r=0;

    if(argc < 2) {
	printf("Usage:  %s hostname\n", argv[0]);
	exit(1);
    }

    r+=rstat(argv[1], &statp[0]);
    sleep(1);
    r+=rstat(argv[1], &statp[1]);

    if(r == 0) {
	printstatp(statp);
    } else {
       printf("rstat was unsuccessful\n");
    }

    return(0);
}
