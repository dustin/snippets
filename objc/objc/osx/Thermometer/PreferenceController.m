//
//  PreferenceController.m
//  Thermometer
//
//  Created by Dustin Sallings on Mon Mar 24 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import "PreferenceController.h"


@implementation PreferenceController

-(void)startUp:(NSUserDefaults *)d
{
    defaults=d;
    [self showWindow: self];
    [url setStringValue: [defaults objectForKey: @"url"]];
    [frequency setIntValue: [[defaults objectForKey: @"frequency"] intValue]];
}

-(IBAction)ok:(id)sender
{
    [self apply: self];
    [self cancel: self];
}

-(IBAction)cancel:(id)sender
{
    [[self window] orderOut: self];
}

-(IBAction)apply:(id)sender
{
    [defaults setObject: [url stringValue] forKey: @"url"];
    int oldfreq=[[defaults objectForKey: @"frequency"] intValue];
    int freq=[frequency intValue];
    if(oldfreq != freq) {
        NSNumber *n=[[NSNumber alloc] initWithInt: [frequency intValue]];
        [defaults setObject: n forKey: @"frequency"];
        [n release];
    }
}


@end
