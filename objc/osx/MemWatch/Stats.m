//
//  Stats.m
//  MemWatch
//
//  Created by Dustin Sallings on Tue Dec 09 2003.
//  Copyright (c) 2003 __MyCompanyName__. All rights reserved.
//

#import "Stats.h"


@implementation Stats

-(id)initWithDefaults: (NSUserDefaults *)d {
    id rv=[super init];
    defaults=d;
    [d retain];
    return(rv);
}

-(int)memMax {
    return(mem_max);
}

-(int)memTotal {
    return(mem_total);
}

-(int)memFree {
    return(mem_free);
}

-(void)update {
    // NSLog(@"Updating...");
    NSString *urlStr=[defaults objectForKey: @"url"];
    NSURL *src=[[NSURL alloc] initWithString: urlStr];
    NSLog(@"Getting stats from %@", src);
    NSString *memlist=[[NSString alloc] initWithContentsOfURL: src];
    NSArray *memarray=[memlist componentsSeparatedByString:@"\n"];
    mem_max = [[memarray objectAtIndex:1] intValue];
    mem_total = [[memarray objectAtIndex:2] intValue];
    mem_free = [[memarray objectAtIndex:3] intValue];
    [memlist release];
    [src release];
    
    NSLog(@"Got %d/%d/%d", mem_max, mem_total, mem_free);
}


@end
