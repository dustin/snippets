//
//  Watching.h
//  eBayWatch
//
//  Created by Dustin Sallings on Wed Mar 05 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "Item.h"

@interface Watching : NSObject {

    NSMutableArray *contents;

	NSArray *sortDescriptors;

}

// For NSTableDataSource
- (int)numberOfRowsInTableView:(NSTableView *)aTableView;
- (id)tableView:(NSTableView *)aTableView
    objectValueForTableColumn:(NSTableColumn *)aTableColumn
    row:(int)rowIndex;

// Managing the contents
-(void)addItem: (Item *)item;
-(void)removeItem: (int)which;

// Getting the stuff
-(NSEnumerator *)objectEnumerator;

@end
