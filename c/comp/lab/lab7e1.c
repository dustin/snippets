#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/*
 * This takes a string that is the floating point part of the
 * number and returns an integer that is the value of it.
 */
int round(char *what)
{
int blah=0;

	if(strlen(what)>2)
	{
		if(atoi(&what[2])>=5)
			blah=1;
		what[2]=0;
	}
	if(strlen(what)==1)
		blah+=(10*atoi(what));
	else
		blah+=atoi(what);
	return(blah);
}

/*
 * This spells out an integer less than 100
 */
void spellout(int num)
{
static char *tens[]={0, 0, "twenty", "thirty", "forty", "fifty", "sixty",
			"seventy", "eighty", "ninety"};
static char *ones[]={0, "one", "two", "three", "four", "five", "six",
			"seven", "eight", "nine", "ten"};

	if(num>=1000000000)
	{
		printf("A whole lot of ");
		return;
	}

	if(num>=1000000)
	{
		spellout( (num/1000000));
		fputs("million ", stdout);
		num-=( (num/1000000)*1000000);
	}

	if(num>=1000)
	{
		spellout( (num/1000));
		fputs("thosand ", stdout);
		num-=( (num/1000)*1000);
	}

	if(num>=100)
	{
		printf("%s hundred ", ones[num/100]);
		num-=( (num/100)*100);
	}

	if(num>10)
	{
		printf("%s ", tens[num/10]);
		num-=( (num/10)*10);
	}

	if(num>0)
		printf("%s ", ones[num]);
}

/*
 * Print the dollar half
 */
void dollars(char *str)
{
	spellout(atoi(str));
	fputs("dollars", stdout);
}

/*
 * Print the cent half
 */
void cents(char *str)
{
int amount=round(str);
	if(amount>0)
	{
		printf(" and ");
		spellout(amount);
		fputs("cents", stdout);
	}
	puts(".");
}

/*
 * chop the string in two and send the parts away
 */
void chopandspit(char *str)
{
int i, dot;
char *ip, *fp;

	for(i=0; i<strlen(str)&&str[i]!='.'; i++);

	dot=i;
	ip=(char *)malloc((dot+1)*sizeof(char));
	fp=(char *)malloc((strlen(str)-dot+1)*sizeof(char));

	for(i=0; i<dot; i++)
	{
		ip[i]=str[i];
	}
	ip[i]=0;

	for(i=dot+1; i<strlen(str)+1; i++)
	{
		fp[i-dot-1]=str[i];
	}
	fp[i-dot]=0;

	dollars(ip);
	cents(fp);

	free(fp);
	free(ip);
}

/*
 * zero out an array
 */
void zerom(char in[], int i)
{
	for(;i>=0; i--)
		in[i]=0;
}

/*
 * main
 */
void main(void)
{
char in[80];

	zerom(in, 0);

	while(1)
	{
		zerom(in, 0);
		puts("Input a number");
		gets(in);
		if(atof(in)==0)
			break;
		chopandspit(in);
	}
}
