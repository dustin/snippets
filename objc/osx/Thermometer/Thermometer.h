//
//  Thermometer.h
//  Thermometer
//
//  Created by Dustin Sallings on Sat Mar 22 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import <Foundation/Foundation.h>

#define DATA_UPDATED @"DATA_UPDATED"
#define RING_BUFFER_SIZE 10

@interface Thermometer : NSObject {
    float reading;
    NSString *name;
    int tag;
    NSString *url;

    NSMutableArray *lastReadings;
    float trend;

	NSMutableData *responseData;
}

// Initialize this Thermometer
-(id)initWithName:(NSString *)theName url:(NSString *)ustr;

-(void)setReading: (float)r;
-(float)reading;
-(float)trend;
-(void)setName: (NSString *)n;
-(NSString *)name;

-(NSArray *)lastReadings;
-(void)update;

-(int)tag;
-(void)setTag:(int)to;

@end
