/*
 * Copyright 1996 SPY Internetworking
 *
 * $Id: parsecgi.c,v 1.1 1997/08/26 06:00:12 dustin Exp $
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <malloc.h>
#include "cgi.h"

char **split(char c, char *string)
{
 int i, j=0, k=0, length;
 char **ret;
 char *p;

   length=strlen(string);

   p=string+length-1;

   /* how many we got? */
   for(i=0; i<length; i++)
   {
       if(string[i]==c)
       {
	   string[i]=0x00;
	   j++;
       }
   }

   j++;

   ret=(char **)malloc( (j+1) * sizeof(char *));
   ret[j--]=NULL;

   for(;j>=0; j--)
   {
       while(*p && p>=string) p--;
       ret[j]=strdup(p+1);
       p--;
   }

   return(ret);
}

unsigned char makeachar(char a, char b)
{
char c;

    c = (a >= 'A' ? ((a & 0xdf) - 'A')+10 : (a - '0'));
    c *= 16;
    c += (b >= 'A' ? ((b & 0xdf) - 'A')+10 : (b - '0'));

    return(c);
}

int cgiadd(struct cgiform *d, char *name, char *val)
{
    char *n, *v;
    struct cgiform *tmp;
    int i, j=0, size1, size2;

    tmp=(struct cgiform *)malloc(sizeof(struct cgiform));
    if(tmp==NULL) return(1);

    /* NetBSD's malloc freaks if I try to malloc something too small */

    size1=strlen(name);
    if(size1<8) size1=8;

    size2=strlen(val);
    if(size2<8) size2=8;

    n=malloc(size1);
    v=malloc(size2);

    i=0, j=0;
    while(name[i]!=NULL)
    {
	switch(name[i])
	{
	    case '+':
		n[j++]=' '; i++; break;
            case '%':
		n[j++]=makeachar(name[i+1], name[i+2]); i+=3; break;
	    default:
		n[j++]=name[i++];
	}
    }
    n[j]=0x00;

    i=0, j=0;
    while(val[i]!=NULL)
    {
	switch(val[i])
	{
	    case '+':
		v[j++]=' '; i++; break;
            case '%':
		v[j++]=makeachar(val[i+1], val[i+2]); i+=3; break;
	    default:
		v[j++]=val[i++];
	}
    }
    v[j]=0x00;

    tmp->name=n;
    tmp->value=v;

    tmp->next=d->next;
    d->next=tmp;
    return(0);
}

struct cgiform *cgiinit(void)
{
  struct cgiform *d;
  char *formdata;
  char **tmp1, **tmp2;
  int length;
  int i, j;

  d=(struct cgiform *)malloc(sizeof(struct cgiform));
  d->next=0;

  if(strcmp(getenv("REQUEST_METHOD"), "POST") == 0)
  {
      length=atoi(getenv("CONTENT_LENGTH"));
      formdata=malloc(length+1);
      memset(formdata, 0x00, length+1);

      fread(formdata, length, 1, stdin);
  }
  else
  {
      length=strlen(getenv("QUERY_STRING"));

      formdata=malloc(length+1);
      strcpy(formdata, getenv("QUERY_STRING"));
      formdata[length+1]=0x00;
  }

  tmp1=split('&', formdata);

  for(i=0; tmp1[i]!=NULL; i++)
  {
     tmp2=split('=', tmp1[i]);

     cgiadd(d, tmp2[0], tmp2[1]);

     free(tmp2[0]);
     free(tmp2[1]);
     free(tmp2);
  }

  for(i=0; tmp1[i]!=NULL; i++)
  {
      free(tmp1[i]);
  }
  free(tmp1);

  return(d);
}
