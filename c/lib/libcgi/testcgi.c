#include <stdio.h>
#include "cgi.h"

void main(void)
{
  struct cgiform *d;

  puts("Content-type: text/plain\n");

  d=cgiinit();

  printf("The %s method was used\n\n", getenv("REQUEST_METHOD"));

  puts("Print the list of variables with their data:");

  cgiprintdata(d);

  printf("\nNow let's grab our own data:\nname2 = %s\n",
	cgigetdata(d, "name2"));

  cgifree(d);
}
