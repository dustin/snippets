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
	// This is the URL from the config
    NSString *urlString;
    float updateInterval;
	// This is an array of URLs that represent all of the images we want to see
	NSArray *imageUrls;
	int currentURLOffset;
    NSImage *currentImage;
    bool blankOnDraw;
    float opacity;

    NSRect screenFrame;

    IBOutlet NSSlider *intervalField;
    IBOutlet NSTextField *updateIntervalLabel;
    IBOutlet NSSlider *opaqueField;
    IBOutlet NSTextField *opaqueLabel;
    IBOutlet NSTextField *urlField;
    IBOutlet NSWindow *sheet;
    IBOutlet NSButton *blankField;
}

- (void)fetchImage;
- (void)setImageURLs;
- (void)drawCurrentImage;
- (void)updateDisplay;

- (IBAction)cancelButton:(id)sender;
- (IBAction)intervalChanged:(id)sender;
- (IBAction)opacityChanged:(id)sender;
- (IBAction)okButton:(id)sender;
@end
