#include <stdio.h>

#define check(x,y) ( (x>=0) && (x<7) && (y>=0) && (y<7) )

void runaround(int x, int y, int a[][7])
{
static v[]={0, 1, 1, 1, 0, -1, -1, -1};
static h[]={-1, -1, 0, 1, 1, 1, 0, -1};
int i;

	for(i=0; i<8; i++)
	{
		if( check(v[i]+y,h[i]+x) )
		{
			printf("%d ", a[h[i]+x][v[i]+y]);
		}
	}
	putchar('\n');
}

void init(int a[][7])
{
int i, j;

	for(i=0;i<7;i++)
		for(j=0;j<7;j++)
			a[i][j]=i*7+j+1;
}

void main(int argc, char *argv[])
{
int x=0, y=0, a[7][7];

	init(a);

	puts("Enter two numbers between 0 and 7");
	scanf("%d %d", &x, &y);
	if(check(x, y)) runaround(x, y, a);
	while(check(x, y))
	{
		puts("Enter two more numbers between 0 and 7");
		scanf("%d %d", &x, &y);
		if(check(x, y)) runaround(x, y, a);
	}
}
