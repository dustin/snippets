//
//  ThermController.h
//  Thermometer
//
//  Created by Dustin Sallings on Sat Mar 22 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <AppKit/AppKit.h>

#import "ThermometerCell.h"

#define SAMPLE_RATE 60

@interface ThermController : NSWindowController {
    IBOutlet NSMatrix *thermMatrix;
    IBOutlet NSTextField *status;
    IBOutlet NSMenu *dockMenu;

    NSMutableArray *therms;
}

-(void)update;
-(IBAction)update:(id)sender;

@end
