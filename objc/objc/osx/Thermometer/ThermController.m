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

    // Now, verify the timer is scheduled appropriately
    double erval=[[defaults objectForKey: @"frequency"] doubleValue];
    double cur=(double)[updater timeInterval];
    if(erval != cur) {
        NSLog(@"Time has changed from %.2f to %.2f, updating", cur, erval);
        [updater invalidate];
        [self scheduleTimer];
    }
}

-(IBAction)launchPreferences:(id)sender
{
    id prefc=[[PreferenceController alloc] initWithWindowNibName: @"Preferences"];
    [prefc startUp: defaults];
    NSLog(@"Initialized Test");
}

-(IBAction)setCelsius:(id)sender
{
    [defaults setObject: @"c" forKey: @"units"];
    [thermMatrix setNeedsDisplay: true];
}

-(IBAction)setFarenheit:(id)sender
{
    [defaults setObject: @"f" forKey: @"units"];
    [thermMatrix setNeedsDisplay: true];
}

// Updates from the UI
-(IBAction)update:(id)sender
{
    [self update];
}

-(void)initDefaults
{
    // Get the default defaults
    NSMutableDictionary *dd=[[NSMutableDictionary alloc] initWithCapacity: 4];
    [dd setObject: @"c" forKey: @"units"];
    [dd setObject: @"http://bleu.west.spy.net/therm/Temperature" forKey: @"url"];
    NSNumber *n=[[NSNumber alloc] initWithInt: 60];
    [dd setObject: n forKey: @"frequency"];
    [n release];

    defaults=[NSUserDefaults standardUserDefaults];
    // Add the default defaults
    [defaults registerDefaults: dd];
    // [self setUnits: [defaults objectForKey: @"units"]];
    [dd release];
}

-(void)scheduleTimer
{
    // Schedule updates
    int freq=[[defaults objectForKey: @"frequency"] intValue];
    NSLog(@"Scheduling timer with frequency:  %d", freq);
    updater=[NSTimer scheduledTimerWithTimeInterval:freq
        target: self
        selector: @selector(update)
        userInfo:nil repeats:true];
}

-(void)awakeFromNib
{
    NSLog(@"Starting ThermController.");

    // Initialize the defaults
    [self initDefaults];

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

    // Grab the list.
    NSString *thermsurls=[[NSString alloc]
        initWithFormat: [defaults objectForKey: @"url"]];
    NSURL *thermsurl=[[NSURL alloc] initWithString: thermsurls];
    NSString *thermlist=[[NSString alloc] initWithContentsOfURL: thermsurl];
    NSArray *thermarray=[thermlist componentsSeparatedByString:@"\n"];
    [thermsurl release];
    [thermlist release];

    therms=[[NSMutableArray alloc] initWithCapacity: [thermarray count]];
    NSEnumerator *enumerator = [thermarray objectEnumerator];
    id anObject;
    while (anObject = [enumerator nextObject]) {
        if([anObject length] > 0) {
            Thermometer *t=[[Thermometer alloc] initWithName: anObject
                url:[defaults objectForKey: @"url"]];
            [therms addObject: t];
            [t release];
        }
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

        [tc setCImage: ci];
        [tc setFImage: fi];
        [tc setDefaults: defaults];
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
    newdims.size.height=223+(151*(c-ocol));
    [[self window] setMinSize: newdims.size];
    [[self window] setMaxSize: newdims.size];
    [[self window] setFrame:newdims display:true];

    // Later initialization
    [self performSelector: @selector(update)
        withObject:nil
        afterDelay:0];

    [self scheduleTimer];
}

@end
