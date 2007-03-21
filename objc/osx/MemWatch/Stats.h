//
//  Stats.h
//  MemWatch
//
//  Created by Dustin Sallings on Tue Dec 09 2003.
//  Copyright (c) 2003 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface Stats : NSObject {
    int mem_max;
    int mem_total;
    int mem_free;
    
    NSUserDefaults *defaults;
}

-(id)initWithDefaults: (NSUserDefaults *)d;

-(int)memMax;
-(int)memTotal;
-(int)memFree;
-(void)update;

@end
