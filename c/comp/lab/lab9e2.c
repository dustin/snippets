#include <stdio.h>
#include <string.h>
#include <bstring.h>

typedef struct {
	char name[21];
	char id[13];
	int age;
} record;

static void printrec(record what)
{
	printf("%-25s%-14s%-3d\n", what.name, what.id, what.age);
}

static void swap(record *r1, record *r2)
{
record t;

	t=*r1;
	*r1=*r2;
	*r2=t;
}

static void dasort(record r[], int n)
{
int i, j;

	for(i=n; i>=0; i--)
		for(j=1; j<i; j++)
			if(strcmp(r[j-1].name, r[j].name)>0)
			{
				swap(&r[j-1], &r[j]);
			}
}

static int input(record r[])
{
int i;
char tmp[20];

	for(i=0; i<100; i++)
	{
		puts("Input the name");
		gets(r[i].name);

		puts("Input the id");
		gets(r[i].id);

		puts("Input the age");
		gets(tmp);
		sscanf(tmp, "%d", &r[i].age);

		if(r[i].age==0) break;
	}
	return i;
}

void main(void)
{
record r[100];
int i, j;

	j=input(r);

	dasort(r, j);
	printf("%-25s%-14s%3s\n", "Name", "ID", "Age");
	for(i=0; i<j; i++)
		printrec(r[i]);
}
