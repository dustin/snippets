//
//  PreferenceController.h
//  MemWatch
//
//  Created by Dustin Sallings on Thu Dec 11 2003.
//  Copyright (c) 2003 __MyCompanyName__. All rights reserved.
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
