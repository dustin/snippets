//
//  SizeScaler.h
//  PhotoUpload
//
//  Created by Dustin Sallings on Sun Oct 06 2002.
//  Copyright (c) 2002 SPY internetworking. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface SizeScaler : NSObject {
    NSSize baseSize;
}

-initWithSize: (NSSize)base;

-(NSSize)scaleTo:(NSSize)size;

@end
