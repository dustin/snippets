//
//  WebSaverView.m
//  WebSaver
//
//  Created by Dustin Sallings on Sun Jul 27 2003.
//  Copyright (c) 2003, SPY internetworking. All rights reserved.
//

#import "WebSaverView.h"


@implementation WebSaverView

- (id)initWithFrame:(NSRect)frame isPreview:(BOOL)isPreview
{
    self = [super initWithFrame:frame isPreview:isPreview];

    ScreenSaverDefaults *defaults=
        [ScreenSaverDefaults defaultsForModuleWithName:@"WebSaver"];
    urlString = [defaults stringForKey:@"url"];
    if(urlString == nil) {
        urlString = @"http://bleu.west.spy.net/~dustin/";
        [defaults setObject: urlString forKey: @"url"];
    }
    updateInterval=[defaults floatForKey:@"interval"];
    if(updateInterval < 1.0) {
        updateInterval=60.0;
        [defaults setFloat: updateInterval forKey: @"interval"];
    }

    // Set the update interval
    if (self) {
        [self setAnimationTimeInterval:updateInterval];
    }

    webview=[[WebView alloc] initWithFrame:frame frameName:nil groupName:nil];
    [self addSubview: webview];

    return self;
}

- (void)startAnimation
{
    [super startAnimation];
}

- (void)stopAnimation
{
    [super stopAnimation];
}

- (void)drawRect:(NSRect)rect
{
    [super drawRect:rect];
}

- (void)animateOneFrame
{
    NSLog(@"Loading %@", urlString);
    NSURL *url=[[NSURL alloc] initWithString: urlString];
    NSURLRequest *req=[[NSURLRequest alloc] initWithURL: url];
    [[webview mainFrame] loadRequest: req];
    [req release];
    [url release];
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
        [ScreenSaverDefaults defaultsForModuleWithName:@"WebSaver"];

    urlString=[urlField stringValue];
    updateInterval=[intervalField floatValue];
    [self setAnimationTimeInterval:updateInterval];

    [defaults setObject: urlString forKey:@"url"];
    [defaults setFloat: updateInterval forKey:@"interval"];
    [defaults synchronize];
    [NSApp endSheet: sheet];
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
