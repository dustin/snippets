//
//  RemoteObject.h
//  TestTool
//
//  Created by Dustin Sallings on Sun Sep 29 2002.
//  Copyright (c) 2002 SPY internetworking. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface RemoteObject : NSObject {
    int _access;
}

-(int)access;

@end
