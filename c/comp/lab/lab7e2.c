#include <stdio.h>

int subset(int a1[], int s1, int a2[], int s2)
{
int ret=0;
int i, j;

	for(i=0; i<s2; i++)
		for(j=0; j<s1; j++)
			ret+=(a1[j]==a2[i]);

	return ret;
}

void main(void)
{
int nums[20], in[5];
int i, flag;

	/* initialize to 1-20 */
	for(i=0; i<20; i++)
		nums[i]=i+1;
	in[0]=2600;

	while(1)
	{
		puts("Input five numbers");
		for(i=0; i < 5; i++)
			scanf("%d", &in[i]);

		flag=0;
		for(i=0; i<5; i++)
			if(in[i]!=0)
				flag++;

		if(flag==0)
			break;

		if(flag=(5-subset(nums, 20, in, 5)))
			if(flag==1)
				printf("%d number is not there\n", flag);
			else
				printf("%d numbers are not there\n", flag);
		else
			puts("All of the numbers are there");
	}
}
