//
//  ImageSaverView.m
//  ImageSaver
//
//  Created by Dustin Sallings on Sat Jul 26 2003.
//  Copyright (c) 2003, SPY internetworking. All rights reserved.
//

#include <stdlib.h>

#import "ImageSaverView.h"
#import "SizeScaler.h"


@implementation ImageSaverView

- (id)initWithFrame:(NSRect)frame isPreview:(BOOL)isPreview
{
    NSLog(@"Initializing");
    self = [super initWithFrame:frame isPreview:isPreview];
    if (self) {
        [self setAnimationTimeInterval:5.0];
    }

    ScreenSaverDefaults *defaults =
        [ScreenSaverDefaults defaultsForModuleWithName:@"ImageSaver"];
    urlString = [defaults stringForKey:@"url"];
    if(urlString == nil) {
        urlString = @"http://bleu.west.spy.net/~dustin/images/mountain.jpg";
        [defaults setObject: urlString forKey: @"url"];
    }
    updateInterval=[defaults floatForKey:@"interval"];
    if(updateInterval < 5.0) {
        updateInterval=5.0;
        [defaults setFloat: updateInterval forKey: @"interval"];
    }

    // Fetch the initial image
    [self fetchImage];

    // Setting timer
    updateTimer=[NSTimer scheduledTimerWithTimeInterval:updateInterval
        target: self
        selector: @selector(fetchImage)
        userInfo:nil repeats:true];

    return self;
}

- (void)startAnimation
{
    NSLog(@"Animation starting");
    [super startAnimation];
}

- (void)stopAnimation
{
    [super stopAnimation];
}

- (NSPoint)getRandomPoint: (NSSize) outer inner: (NSSize) inner
{
    NSPoint rv;
    int diffx=inner.width - outer.width;
    int diffy=inner.height - outer.height;
    // NSLog(@"Image size diff is %dx%d", diffx, diffy);
    int distancex=random()%diffx;
    int distancey=random()%diffy;

    rv=NSMakePoint(0.0+distancex, 0.0+distancey);
    // NSLog(@"Distance is %dx%d", distancex, distancey);
    return(rv);
}

- (void)drawRect:(NSRect)rect
{
    // NSLog(@"Drawing.");
    [super drawRect:rect];
    if(currentImage != nil) {
        // Make sure the image has been scaled.
        NSSize imageSize=[currentImage size];
        if(imageSize.width > rect.size.width || imageSize.height > rect.size.height) {
            SizeScaler *ss=[[SizeScaler alloc] initWithSize: [currentImage size]];
            imageSize=[ss scaleTo: rect.size];
            [currentImage setSize: imageSize];
        }
        NSPoint p=[self getRandomPoint: rect.size inner: imageSize];
        NSRect theRect=NSMakeRect(0, 0, imageSize.width, imageSize.height);
        [currentImage drawAtPoint: p fromRect: theRect
            operation: NSCompositeCopy fraction: 1.0];
    }
}

- (void)fetchImage
{
    NSURL *u=[[NSURL alloc] initWithString: urlString];
    NSAutoreleasePool *pool=[[NSAutoreleasePool alloc] init];
    NSLog(@"Fetching image from %@", u);
    NSData *data=[u resourceDataUsingCache: FALSE];

    if(data != nil) {
        NSImage *tmpImage=[[NSImage alloc] initWithData: data];
        if(tmpImage != nil) {
            [tmpImage setScalesWhenResized: TRUE];
            if(currentImage != nil) {
                NSLog(@"Releasing old image");
                [currentImage release];
            }
            currentImage=tmpImage;
        }
    }

    [self setNeedsDisplay: TRUE];
    [u release];
    [pool release];
}

- (void)animateOneFrame
{
    // NSLog(@"Animating");
    [self setNeedsDisplay: YES];
    return;
}

- (BOOL)hasConfigureSheet
{
    return YES;
}

- (NSWindow*)configureSheet
{
    if(sheet == nil) {
        NSLog(@"Loading sheet.");
        [NSBundle loadNibNamed:@"ConfigPanel" owner:self];
    }
    [urlField setStringValue: urlString];
    // Update the interval display
    [intervalField setFloatValue: updateInterval];
    [self intervalChanged: self];
    return sheet;
}

- (IBAction) okButton:(id)sender
{
    NSLog(@"OK hit");
    ScreenSaverDefaults *defaults =
        [ScreenSaverDefaults defaultsForModuleWithName:@"ImageSaver"];

    urlString=[urlField stringValue];
    float tmpF=[intervalField floatValue];
    if(tmpF != updateInterval) {
        [defaults setFloat: tmpF forKey:@"interval"];
        if(updateTimer != nil) {
            NSLog(@"Canceling old timer");
            [updateTimer invalidate];
        }
        NSLog(@"Scheduling a new timer");
        updateTimer=[NSTimer scheduledTimerWithTimeInterval:updateInterval
            target: self
            selector: @selector(fetchImage)
            userInfo:nil repeats:true];
    }

    [defaults setObject: urlString forKey:@"url"];
    [defaults synchronize];
    [NSApp endSheet: sheet];
    [self fetchImage];
}

- (IBAction) cancelButton:(id)sender
{
    NSLog(@"Cancel hit");
    [NSApp endSheet: sheet];
}

- (IBAction)intervalChanged:(id)sender
{
    // NSLog(@"Interval changed.");
    int secs=[intervalField intValue];
    int hours=0;
    int minutes=0;
    if(secs > 3600) {
        hours=secs/3600;
        secs=secs % 3600;
    }
    if(secs>60) {
        minutes=secs/60;
        secs=secs%60;
    }
    // OK, now let's make a label
    NSAutoreleasePool *pool=[[NSAutoreleasePool alloc] init];
    NSString *label=@"";
    if(hours > 0) {
        if(hours == 1) {
            label=@"1 hour";
        } else {
            label=[label stringByAppendingFormat: @"%d hours", hours];
        }
    }
    if(minutes > 0) {
        if(minutes == 1) {
            label=[label stringByAppendingString: @" 1 minute"];
        } else{
            label=[label stringByAppendingFormat: @" %d minutes", minutes];
        }
    }
    if(secs > 0) {
        if(secs == 1) {
            label=[label stringByAppendingString: @" 1 second"];
        } else {
            label=[label stringByAppendingFormat: @" %d seconds", secs];
        }
    }
    [updateLabel setStringValue: label];
    [pool release];
}

@end
