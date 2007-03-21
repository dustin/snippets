#include <iostream.h>
#include <fstream.h>

#define J 43

int w, answer[3];
int possible[J];

void init(void)
{
int i, j;

	for(i=0,j=0;i<=20;i++)
	{
		possible[j++]=i;
	}
	for (i=21;i<=40;i++)
	{
  		if (i%2 ==0 || i%3==0)
		possible[j++]=i;
	}
	for (i=41;i<=60;i++)
	{
		if (i%3==0 || i==50)
		possible[j++]=i;
	}
}

void comb(int x, int max=J)
{
int i, j, k;
}

int main(int argc, char *argv[])
{
fstream fin;
int x;

	init();
	fin.open("dart.in", ios::in);
	fin >> x;
	comb(x);

	return 0;
}
