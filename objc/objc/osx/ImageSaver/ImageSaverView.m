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
        urlString = @"http://bleu.west.spy.net/therm/House";
        [defaults setObject: urlString forKey: @"url"];
    }
    updateInterval=[defaults floatForKey:@"interval"];
    if(updateInterval < 5.0) {
        updateInterval=5.0;
        [defaults setFloat: updateInterval forKey: @"interval"];
    }

	[self setImageURLs];

	// Update right away
	[self performSelector: @selector(fetchImage)
        withObject:nil afterDelay:1.0];

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

- (void)shuffleArray: (NSMutableArray *)a
{
	int i, j, n;
	id d;
	n = [a count] - 1;
	for(i = n; i>=0; i--) {
		j=random() % n;
		if(j == i) {
			continue;
		}
		d = [[a objectAtIndex:i] retain];
		[a replaceObjectAtIndex:i withObject:[a objectAtIndex:j]];
		[a replaceObjectAtIndex:j withObject:d];
		[d release];
	}
}

- (void)setImageURLs
{
    NSURL *u=[[NSURL alloc] initWithString: urlString];
    NSAutoreleasePool *pool=[[NSAutoreleasePool alloc] init];
    NSLog(@"Fetching url list from %@", u);
    NSData *data=[u resourceDataUsingCache: FALSE];

	char fourbytes[5];
	[data getBytes: &fourbytes length: 4];
	fourbytes[4]=0x00;

	NSLog(@"First four bytes are %x.%x.%x.%x",
		fourbytes[0], fourbytes[1], fourbytes[2], fourbytes[3]);

	NSArray *newList=nil;
	if(strcmp(fourbytes, "http") == 0) {
		NSLog(@"Hey, that says http, I'm guessing this is a list of URLs");
		NSString *str=[[NSString alloc] initWithData: data
			encoding:NSUTF8StringEncoding];
		NSArray *splitList=[str componentsSeparatedByString: @"\n"];
		NSMutableArray *mutList=[[NSMutableArray alloc] initWithCapacity: [splitList count]];

		// Copy the things that look like URLs
		NSEnumerator *enumerator = [splitList objectEnumerator];
		id anObject=nil;
		while (anObject = [enumerator nextObject]) {
			if([anObject hasPrefix: @"http"]) {
				[mutList addObject: anObject];
			}
		}
		[self shuffleArray: mutList];
        newList=mutList;
	} else {
		NSLog(@"Doesn't look like a URL list, assuming it's an image");
		newList=[[NSArray alloc] initWithObjects: urlString, nil];
	}

	// Release the old one if it's still there.
	if(imageUrls != nil) {
		[imageUrls release];
	}
	imageUrls=newList;
	currentURLOffset=0;

	NSLog(@"URL list:  %@", imageUrls);

	[pool release];
}

- (void)fetchImage
{
	currentURLOffset++;
	if(currentURLOffset >= [imageUrls count]) {
		NSLog(@"Resetting URL offset to 0");
		currentURLOffset=0;
	}
	NSLog(@"URL offset is %d", currentURLOffset);

    NSURL *u=[[NSURL alloc]
		initWithString: [imageUrls objectAtIndex: currentURLOffset]];
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
    } else {
		NSLog(@"Failed to fetch image.");
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
    [self setImageURLs];
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
