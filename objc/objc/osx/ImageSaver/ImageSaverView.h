//
//  ImageSaverView.h
//  ImageSaver
//
//  Created by Dustin Sallings on Sat Jul 26 2003.
//  Copyright (c) 2003, SPY internetworking. All rights reserved.
//

#import <ScreenSaver/ScreenSaver.h>

@interface ImageSaverView : ScreenSaverView
{
    NSString *urlString;
    float updateInterval;
    NSImage *currentImage;

    NSTimer *updateTimer;

    float distancex;
    float distancey;

    IBOutlet NSSlider *intervalField;
    IBOutlet NSTextField *updateLabel;
    IBOutlet NSTextField *urlField;
    IBOutlet NSWindow *sheet;
}

- (void)fetchImage;

- (IBAction)cancelButton:(id)sender;
- (IBAction)intervalChanged:(id)sender;
- (IBAction)okButton:(id)sender;
@end
