#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define fd(a,b) (finddot(a)-finddot(b))
#define sd(a,b) (strlen(a)-strlen(b))
#define sfd(a) (strlen(a)-finddot(a))
#define getwidth(a,b) ((strlen(a)>strlen(b))?strlen(a):strlen(b))
#define numodecs(a,b) (sfd(a)>sfd(b)?sfd(a):sfd(b))

FILE *in, *out;
int lastdigit;

int finddot(char *blah)
{
int i;
  for(i=0; i<strlen(blah); i++)
    if(blah[i]=='.') return i;
  return -1;
}

void pad(char *a, char *b)
{
char tmpa[161], tmpb[161];

  memset(tmpa, ' ', 160); memset(tmpb, ' ', 160);

  if(fd(b,a) > 0)	tmpa[fd(b,a)+1]=0x00;
  else			tmpa[2]=0x00;

  if(fd(a,b) > 0)	{ tmpb[fd(a,b)+2]=0x00; tmpb[fd(a,b)]='+'; }
  else			{ tmpb[2]=0x00; tmpb[0]='+'; }

  strcat(tmpa, a); strcat(tmpb, b);

  if(strlen(tmpa)>strlen(tmpb)) {
    memset(b, ' ', sd(tmpa, tmpb)); b[sd(tmpa, tmpb)]=0x00;
    strcat(tmpb, b);
  }

  if(strlen(tmpb)>strlen(tmpa)) {
    memset(a, ' ', sd(tmpb, tmpa)); a[sd(tmpb, tmpa)]=0x00;
    strcat(tmpa, a);
  }

  strcpy(a, tmpa); strcpy(b, tmpb);
}

int add(char a, char b)
{
int i, j;

  i=((a==' ' || a=='+')?0:a-48);
  j=((b==' ' || b=='+')?0:b-48);
  return(i+j);
}

void domath(char *a, char *b, int i, int carry)
{
int tmp=0;

  tmp=add(a[i], b[i])+carry;

  if(tmp>0)carry=tmp/10;

  if( (tmp%10>0) && lastdigit==0)
	if(finddot(a)>0)	lastdigit=i+1;
	else			lastdigit=strlen(a);

  if(i>2)domath(a, b, i-1, carry);

  if(i<lastdigit || i<3) {
    if(tmp<0)
      if(i==2)
        if(carry)	fprintf(out, "%s", (i<3?"1":"1."));
        else		fprintf(out, "%s", (i<3?"0":"0."));
      else fputc('.', out);
    else
      if(i==2)		fprintf(out, "%d", (tmp%10)+(carry*10));
      else		fprintf(out, "%d", (tmp%10));
  }
}

void main(void)
{
char a[161], b[161];

  in=fopen("math.dat", "r"); out=fopen("math.out", "w");
  while(1) {
    fgets(a, 80, in); a[strlen(a)-1]=0x00;
    if(NULL==(fgets(b, 80, in)))break;
      b[strlen(b)-1]=0x00;

    pad(a, b); lastdigit=0;
    fprintf(out, "%s\n%s\n= ", a, b);
    domath(a, b, strlen(a)-1, 0);
    fputs("\n\n", out);
  }
  fclose(in); fclose(out);
}
