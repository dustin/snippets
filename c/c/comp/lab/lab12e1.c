#include <stdio.h>
#include <stdlib.h>

/*
 * Record structure is defined as follows:
 */

struct rec {
	char name[25];
	char id[15];
	int hours;
	float gpa;
};

/*
 * This will be output to a binary file since the
 * size of the structure is constant and known.
 *
 * This will be accomplished by reading in all of the
 * data and immediately writing it to a binary file if
 * the data is desirable.
 *
 * My routine to get a record will return a pointer to
 * a record.
 */

void myscanf(char *fmt, void *arg)
{
char buf[80];
int i;

	for(i=0; i<80; i++) buf[i]=0;

	gets(buf);
	switch(fmt[1])
	{
		case 'f':
			sscanf(buf, fmt, (float *)arg);
			break;
		case 'd':
			sscanf(buf, fmt, (int *)arg);
			break;
	}
}

struct rec *getrec(void)
{
struct rec *tmp;
	tmp=(struct rec *)malloc(sizeof(struct rec));

	puts("Input name:");
	gets(tmp->name);
	puts("Input id:");
	gets(tmp->id);
	puts("Input number of hours taken");
	myscanf("%d", &(tmp->hours));
	puts("Input gpa");
	myscanf("%f", &(tmp->gpa));
	return(tmp);
}

void writerec(struct rec *r, FILE *out)
{
	fwrite(r, sizeof(struct rec), 1, out);
	free(r); /* and throw it away */
}


void doit(int i)
{
FILE *out;

	if((out=fopen("Lab12.Dat", "wb"))==NULL)
	{
		perror("fopen");
		exit(1);
	}
	for(;i>0;i--)
	{
		writerec(getrec(), out);
	}
	fclose(out);
}

void main(void)
{
int i;

	puts("How many records do you want to put in there?");
	myscanf("%d", &i);
	if(i>0)
		doit(i);
}
