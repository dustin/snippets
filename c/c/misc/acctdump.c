#include <stdio.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/acct.h>
#include <sys/param.h>

#ifndef ACCT_FILE
#define ACCT_FILE "/var/adm/pacct"
#endif /* ACCT_FILE */

int main(int argc, char **argv)
{
	struct acct astruct;
	FILE *f;
	double stick;

	stick=(double)TICK/100000.0;

	f=fopen(ACCT_FILE, "rb");
	assert(f);

	while(fread(&astruct, sizeof(astruct), 1, f)>0) {
		printf("%-8s(%d)\tuid=%d\tt=%.02fr,%.02fu,%.02fs\t%s",
			astruct.ac_comm, astruct.ac_stat,
			astruct.ac_uid,
			(double)astruct.ac_etime/stick,
			(double)astruct.ac_utime/stick,
			(double)astruct.ac_stime/stick,
			ctime(&astruct.ac_btime));
	}

	fclose(f);
	exit(0);
}
