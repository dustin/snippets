//
//  Watching.m
//  eBayWatch
//
//  Created by Dustin Sallings on Wed Mar 05 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import "Watching.h"
#import "Controller.h"

@implementation Watching

-(void)sortData
{
	NSLog(@"Resorting data");
	[contents sortUsingDescriptors: sortDescriptors];
}

- (id)init
{
    contents=[[NSMutableArray alloc] initWithCapacity: 10];

	[[NSNotificationCenter defaultCenter]
		addObserver:self
		selector:@selector(sortData)
		name:DATA_UPDATED
		object:nil];

    return(self);
}

- (int)numberOfRowsInTableView:(NSTableView *)aTableView
{
    // NSLog(@"Asking for the number of rows (%d)", [contents count]);
    return([contents count]);
}

- (id)tableView:(NSTableView *)aTableView
    objectValueForTableColumn:(NSTableColumn *)aTableColumn
    row:(int)rowIndex
{
    // NSLog(@"Asking for row %d of %@", rowIndex, [aTableColumn identifier]);
    id theItem=[contents objectAtIndex: rowIndex];
	id rv=[theItem valueForKey:[aTableColumn identifier]];

    return(rv);
}

- (void)tableView:(NSTableView *)tableView
	sortDescriptorsDidChange:(NSArray *)oldDescriptors
{
	NSLog(@"Changing sorting descriptors");
	if(sortDescriptors != nil) {
		[sortDescriptors release];
	}
	sortDescriptors=[tableView sortDescriptors];
	[sortDescriptors retain];
	[self sortData];
}

-(void)addItem: (Item *)item
{
    NSLog(@"Adding %@", item);
    [contents addObject: item];
	[self sortData];
}

-(void)removeItem: (int)which
{
	NSLog(@"Removing %d", which);
	[contents removeObjectAtIndex: which];
}

-(NSEnumerator *)objectEnumerator
{
    return([contents objectEnumerator]);
}


@end
