//
//  Thermometer.m
//  Thermometer
//
//  Created by Dustin Sallings on Sat Mar 22 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import "Thermometer.h"


@implementation Thermometer

-(id)initWithName:(NSString *)theName url:(NSString *)ustr;
{
	// Super initialization
    id rv=[super init];
	// Set the name
    name=theName;
    [name retain];
	// Set the update URL
    url=ustr;
    [url retain];
    _t_delegate=nil;
	// Initialize the array of previous readings
    lastReadings=[[NSMutableArray alloc] initWithCapacity: 10];
	// And return
    return(rv);
}

- (void)dealloc
{
    [lastReadings release];
    [name release];
    [super dealloc];
}

-(void)setValidReading:(float)r
{
    // Normal reading update stuff
    if(reading != r) {
        float oldreading=reading;
        reading=r;
        NSLog(@"Updated %@ (%.2f -> %.2f)", [self name], oldreading, reading);

        // Let the delegate know something's changed
        if(_t_delegate != nil) {
            [_t_delegate newReading: r];
        }
    }

    // Keep the array small enough.
    while([lastReadings count] >= RING_BUFFER_SIZE) {
        [lastReadings removeLastObject];
    }
    // Add the current reading
    NSNumber *n=[[NSNumber alloc] initWithFloat: r];
    [lastReadings insertObject:n atIndex: 0];
    [n release];

    // Check to see whether we're going up or down
    n=[lastReadings lastObject];
    // Remember the trend (upwards or downwards)
    trend=r - [n floatValue];
}

// Check for valid values
static bool isValidReading(float r)
{
    return( (r>-100) && (r<100) );
}

-(void)setReading:(float)r
{
    if(isValidReading(r)) {
        [self setValidReading: r];
    }
}

-(float)reading
{
    return(reading);
}

-(float)trend
{
    return(trend);
}

-(int)tag
{
    return(tag);
}

-(void)setTag:(int)to
{
    tag=to;
}

-(void)setName: (NSString *)n
{
    name=n;
    [name retain];
}

-(NSString *)name
{
    return(name);
}

-(NSString *)description
{
    NSString *rv = [NSString stringWithFormat: @"%@ %.2f", name, reading];

    return(rv);
}

-(void)update
{
    NSString *s=[[NSString alloc] initWithFormat: @"%@?temp=%@", url, name];
    NSURL *u=[[NSURL alloc] initWithString: s];
    NSString *sr=[[NSString alloc] initWithContentsOfURL: u];
    [self setReading: [sr floatValue]];
    [s release];
    [u release];
    [sr release];
}

-(NSArray *)lastReadings
{
    return(lastReadings);
}

// Delegate handling
-(void)setDelegate:(id)delegate
{
    _t_delegate=delegate;
}
-(id)delegate
{
    return(_t_delegate);
}


@end
