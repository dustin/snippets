#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "plot.h"
#include "data.h"

int
main(int argc, char *argv[])
{
  FILE *infile;
  int i;
  Head header;
  Point point;

  if (NULL == (infile = fopen(argv[1], "rb")))
    {
      perror(argv[1]);
      exit(1);
    }

  fread(&header, sizeof(header), 1, infile);

  for(i=0; i < header.num_points ; i++)
  {
      fread(&point, sizeof(point), 1, infile);

      printf("%.5f            %.5f\n", point.lat, point.lng);
  }

  exit(0);
}
