//
//  Item.h
//  eBayWatch
//
//  Created by Dustin Sallings on Wed Mar 05 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface Item : NSObject {

    NSString *description;
    float price;
    NSString *itemId;

	NSMutableData *responseData;

}

-(id)initWithId: (NSString *)i description: (NSString *)d price:(float)p;

-(void)setPrice: (float)p;

-(NSString *)itemId;
-(float)price;
-(NSString *)description;

-(void)update;

@end
