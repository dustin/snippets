//
//  RemoteObject.m
//  TestTool
//
//  Created by Dustin Sallings on Sun Sep 29 2002.
//  Copyright (c) 2002 SPY internetworking. All rights reserved.
//

#import "RemoteObject.h"


@implementation RemoteObject

-(int)access
{
    _access++;
    NSLog(@"Server access, returning %d\n", _access);
	sleep(5);
    return(_access);
}

@end
