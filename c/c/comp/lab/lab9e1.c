/* The following program does not have any syntax error. When running,
howver, records entered are not printed out in alphabetical order according
to name field. You are going to  debug this program. The new program should
do:
1. The output format should look like:
Name                    ID              Age
Adam                    12345..         25
...
2. All the records entered must be printed out in sorted order.
*/

#include<stdio.h>
#include<string.h>
typedef struct
{
  char name[21];
  char idnum[13];
  int age;
} recs;

void sort(recs r[], int i);
void print(recs r[], int i);
void main()
{
  recs x[100];
  int i;
  char garbage[5];
  for(i=0; i<100; i++)
  {
	 printf("Please enter a name: ");
	 gets(x[i].name);
	 printf("Please enter your ID number: ");
	 gets(x[i].idnum);
	 printf("Please enter your age: ");
	 scanf("%d", &(x[i].age));
	 gets(garbage);
	 if(x[i].age==0) break;
  }
  sort(x, i);
  print(x, i);
}
void sort(recs r[], int size)
{
  int i,j,k;
  recs t;
  for(i=0; i<size-1;i++)
  {
	 k=i;
	 for(j=i+1; j<size; j++)
	      if(strcmp(r[i].name, r[j].name)>0)
	      {
		k=j;
	        t= r[i];
	        r[i]=r[k];
	        r[k]=t;
	      }
  }
}
void print(recs s[], int size)
{
  int i;
  puts("NAME                 ID           AGE");

  for(i=0; i<size; i++)
  {
	 printf("%-20s %-13s%d\n", s[i].name, s[i].idnum, s[i].age);
  }
}
