#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "plot.h"
#include "data.h"

char newname[61];

char *
getfilename(char *infilename)
{
  int i;

  for (i = 0; i < strlen(infilename); i++)
    {
      if (infilename[i] == '.')
	break;
    }

  strncpy(newname, infilename, i);

  strcat(newname, ".bin");

  return (newname);
}

int
main(int argc, char *argv[])
{
  char *infilename, line[81];
  FILE *infile, *outfile;
  Head header;
  Point point;

  if (argc < 2)
    infilename = DEFAULTFILENAME;
  else
    infilename = argv[1];

  if (NULL == (infile = fopen(infilename, "r")))
    {
      perror(infilename);
      exit(1);
    }

  if (NULL == (outfile = fopen(getfilename(infilename), "wb")))
    {
      perror(getfilename(infilename));
      exit(1);
    }

  strcpy(header.magic, BINMAGIC);

  header.min_lat = 9999999.0;
  header.max_lat = -999999.0;
  header.min_lng = 9999999.0;
  header.max_lng = -999999.0;
  header.num_points = 0;

  while (!feof(infile))
    {
      fgets(line, 80, infile);

      if (feof(infile))
	continue;

      point.lat = atof(line);
      point.lng = atof(line + 12);

      if (point.lat > -70)
	continue;

      if (point.lat < header.min_lat)
	header.min_lat = point.lat;
      if (point.lat > header.max_lat)
	header.max_lat = point.lat;

      if (point.lng < header.min_lng)
	header.min_lng = point.lng;
      if (point.lng > header.max_lng)
	header.max_lng = point.lng;

      header.num_points++;
    }

  header.lng_diff = header.max_lng - header.min_lng;
  header.lat_diff = header.max_lat - header.min_lat;

  fwrite(&header, sizeof(header), 1, outfile);

  fprintf(stderr, "%d points found.\n", header.num_points);

  rewind(infile);

  while (!feof(infile))
    {
      fgets(line, 80, infile);

      if (feof(infile))
	continue;

      point.lat = atof(line);
      point.lng = atof(line + 12);

      fwrite(&point, sizeof(point), 1, outfile);
    }

  exit(0);
}
