#ifndef _DATA_H_
#define _DATA_H_

#define BINMAGIC "PLT1.0"

#define DATAFILE "coast.bin"

typedef struct {
	float lng;
	float lat;
} Point;

typedef struct {
	char magic[17];
	int num_points;
	float max_lng;
	float min_lng;
	float max_lat;
	float min_lat;
	float lat_diff;
	float lng_diff;
} Head;

#endif
