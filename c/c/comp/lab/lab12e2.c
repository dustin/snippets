#include <stdio.h>
#include <stdlib.h>

struct rec {
	char name[25];
	char id[15];
	int hours;
	float gpa;
};

struct rec *getrec(int which)
{
struct rec *tmp;
FILE *in;
	if((in=fopen("Lab12.Dat", "rb"))==NULL)
	{
		perror("fopen");
		exit(1);
	}
	tmp=(struct rec *)malloc(sizeof(struct rec));

#ifdef DEBUG
	printf("Getting %d (offset %d)\n", which+1, which*sizeof(struct rec));
#endif
	fseek(in, (which*sizeof(struct rec)), 0);
	fread(tmp, sizeof(struct rec), 1, in);
	fclose(in);
	return(tmp);
}

void printrec(struct rec *r)
{
	printf("Name:\t\t%s\n", r->name);
	printf("ID:\t\t%s\n", r->id);
	printf("Hours:\t\t%d\n", r->hours);
	printf("GPA:\t\t%f\n", r->gpa);
}

void main(void)
{
int i;
struct rec *r;
	puts("Which record do you want to pull out?");
	scanf("%d%*c", &i);
	r=getrec(i-1);
	printrec(r);
	free(r);
}
