//
//  Artist.h
//  OSXMountains
//
//  Created by Dustin Sallings on Tue Apr 08 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Mountain.h"

#define SIDE 1.0
#define BLACK 0
#define WHITE 1
#define SEA_LIT 2
#define SEA_UNLIT 3
#define SKY 4
#define BAND_BASE 5
#define BAND_SIZE 80
#define N_BANDS 3
#define DEF_COL (BAND_BASE + (N_BANDS * BAND_SIZE))
#define MIN_COL (BAND_BASE + (N_BANDS * 2))

@interface Artist : NSObject {

	bool initialized;
	int base;
	Mountain *m;
	NSArray *shadow;
	NSArray *a_strip;
	NSArray *b_strip;

	NSRect bounds;

	NSArray *colors;

	int n_col;
	int band_size;

	/* Width of terrain strip */
	int width;
	/* Level of ambient light */
	float ambient;
	/* Increase or decrease effect of cosine rule */
	double contrast;
	double contour;
	/* Relative strength of vertical light relative to the main light source */
	double vfract;

	double altitude;
	double distance;
	/* Angle of the light */
	double phi;
	/* Angle of the light */
	double alpha;

	/* Offset from calcalt to artist coordinates */
	double base_shift;
	double sealevel;
	double stretch;
	bool draw_map;
	bool reflec;
	int repeat;

	int pos;
	int scroll;

	double shift;
	double varience;
	double delta_shadow;
	double shadow_slip;
	double shadow_register;
	double cos_phi;
	double sin_phi;
	double tan_phi;
	double x_fact;
	double y_fact;
	double vangle;
	double vscale;
	double tan_vangle;
	double viewpos;
	double viewheight;
	double focal;
	double vstrength;
	double lstrength;

}

/* Constructor with the stuff necessary for drawring */
-(id)initWithStuff:(NSRect)bounds mountain:(Mountain *)m;

@end
