//
//  ThermController.m
//  Thermometer
//
//  Created by Dustin Sallings on Sat Mar 22 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import "ThermController.h"


@implementation ThermController

-(void)update
{
    NSLog(@"Updating.");
    NSEnumerator *enumerator = [therms objectEnumerator];
    id object;
    while (object = [enumerator nextObject]) {
        [object update];
        // Update the menu
        [[dockMenu itemWithTag: [object tag]] setTitle: [object description]];
    }
    NSString *s=[[NSString alloc] initWithFormat: @"Last update:  %@",
        [[NSDate date] description]];
    [thermMatrix setNeedsDisplay: true];
    [status setStringValue: s];
    [s release];
}

// Updates from the UI
-(IBAction)update:(id)sender
{
    [self update];
}

-(void)awakeFromNib
{
    NSLog(@"Starting ThermController.");

    NSBundle *mainBundle = [NSBundle mainBundle];
    NSString *path = [mainBundle pathForResource:@"therm-c" ofType:@"png"];
    NSImage *ci = [[NSImage alloc]initWithContentsOfFile:path];
    path = [mainBundle pathForResource:@"therm-f" ofType:@"png"];
    NSImage *fi = [[NSImage alloc]initWithContentsOfFile:path];

    // Get the current number of rows and columns
    int r, c, orow, ocol;
    [thermMatrix getNumberOfRows:&r columns:&c];
    orow=r;
    ocol=c;

    NSArray *array=[[NSArray alloc] initWithObjects:
        @"bedroom", @"livingroom", @"guestroom",
        @"garage", @"newmachineroom", @"backyard", nil];

    therms=[[NSMutableArray alloc] initWithCapacity: [array count]];
    NSEnumerator *enumerator = [array objectEnumerator];
    id anObject;
    while (anObject = [enumerator nextObject]) {
        Thermometer *t=[[Thermometer alloc] initWithName: anObject];
        [therms addObject: t];
        [t release];
    }

    int i;
    for(i=0; i<[therms count]; i++) {
        ThermometerCell *tc=[[ThermometerCell alloc] init];
        [tc setTherm: [therms objectAtIndex: i]];
        [[tc therm] setTag: i];
        NSMenuItem *mi=[[[NSMenuItem alloc] initWithTitle:[tc description]
            action:nil keyEquivalent:@""] autorelease];
        [mi setTag: i];
        [dockMenu addItem: mi];

        [tc setImage: ci];
        // Figure out where to put it
        int rownum, colnum;
        rownum=i % ([therms count]/3);
        colnum=i%3;
        while(colnum>=c) {
            [thermMatrix addColumn];
            c++;
        }
        while(rownum>=r) {
            [thermMatrix addRow];
            r++;
        }

        // NSLog(@"Adding %@ at %d,%d", tc, rownum, colnum);
        [thermMatrix putCell:tc atRow:rownum column:colnum];
        [tc release];
    }
    [thermMatrix sizeToCells];

    NSRect newdims=[[self window] frame];
    newdims.size.width=318+(143*(r-orow));
    newdims.size.height=233+(151*(c-ocol));
    [[self window] setMinSize: newdims.size];
    [[self window] setMaxSize: newdims.size];
    [[self window] setFrame:newdims display:true];

    // Later initialization
    [self performSelector: @selector(update)
        withObject:nil
        afterDelay:0];

    // Schedule updates
    [NSTimer scheduledTimerWithTimeInterval:SAMPLE_RATE
        target: self
        selector: @selector(update)
        userInfo:nil repeats:true];

    // free stuff
    [array release];
}

@end
