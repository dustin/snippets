//
//  PreferenceController.h
//  Thermometer
//
//  Created by Dustin Sallings on Mon Mar 24 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import <AppKit/AppKit.h>


@interface PreferenceController : NSWindowController {

    IBOutlet NSTextField *url;
    IBOutlet NSTextField *frequency;

    NSUserDefaults *defaults;
}

-(IBAction)ok:(id)sender;
-(IBAction)cancel:(id)sender;
-(IBAction)apply:(id)sender;

-(void)startUp:(NSUserDefaults *)d;

@end
