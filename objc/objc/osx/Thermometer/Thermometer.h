//
//  Thermometer.h
//  Thermometer
//
//  Created by Dustin Sallings on Sat Mar 22 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import <Foundation/Foundation.h>

#define RING_BUFFER_SIZE 10

@interface Thermometer : NSObject {
    float reading;
    NSString *name;

    NSMutableArray *lastReadings;
    float trend;

    id _t_delegate;
}

// Initialize this Thermometer
-(id)initWithName: (NSString *)theName;

-(void)setReading: (float)r;
-(float)reading;
-(float)trend;
-(void)setName: (NSString *)n;
-(NSString *)name;

-(NSArray *)lastReadings;
-(void)update;

// Delegate
-(void)setDelegate:(id)delegate;
-(id)delegate;

@end
