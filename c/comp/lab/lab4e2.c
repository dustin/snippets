#include <stdio.h>

int there(int n, int l, int a[])
{
	for(;l>=0;l--)
	{
		if(a[l]==n) return 1;
	}
	return 0;
}

int check(int length, int a[], int out[])
{
int i;
int b=0;
	for(i=0; i<length; i++)
	{
		if(!there(a[i], b, out))
			out[b++]=a[i];
	}
	return b;
}

int readnums(int a[])
{
int i, tmp;
	for(i=0;i<50;i++)
	{
		scanf("%d", &tmp);
		if(tmp==0) { i--; break; }
		a[i]=tmp;
	}
	return i;
}

void main(void)
{
int a[50], newa[50];
int i, length;

	puts("Input up to 50 numbers, end with 0");
	length=readnums(a);
	printf("You gave me %d, I guess that'll do\n", length);
	length=check(length, a, newa);

	for(i=0; i<length; i++)
	{
		printf("%d ", newa[i]);
	}
	putchar('\n');
}
