#ifndef _DATA_H_
#define _DATA_H_

#define BINMAGIC "PLT1.0"

#define DATAFILE "coast.bin"

typedef struct {
	double lng;
	double lat;
} Point;

typedef struct {
	char magic[17];
	int num_points;
	double max_lng;
	double min_lng;
	double max_lat;
	double min_lat;
	double lat_diff;
	double lng_diff;
} Head;

#endif
