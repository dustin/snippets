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

    screenFrame=frame;

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
    blankOnDraw=[defaults boolForKey:@"blankOnDraw"];
    [self setAnimationTimeInterval: updateInterval];
    opacity=[defaults floatForKey:@"opacity"];
    if(opacity <= 0.0 || opacity > 1.0) {
        opacity=1.0;
    }

	[self setImageURLs];

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
    [self drawCurrentImage];
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

    NSURLRequest *theRequest=[NSURLRequest requestWithURL:u
                        cachePolicy:NSURLRequestUseProtocolCachePolicy
                    timeoutInterval:60.0];
    NSURLResponse *theResponse=nil;
    NSError *error=nil;

    NSData *data=[NSURLConnection sendSynchronousRequest: theRequest
        returningResponse:&theResponse error: &error];

    if(data == nil) {
        NSLog(@"Error is %@", error);
    }

    NSLog(@"Mime type of response is %@", [theResponse MIMEType]);

	NSArray *newList=nil;
	if([@"text/plain" isEqual: [theResponse MIMEType]]) {
		NSLog(@"Content type is text/plain, I'm guessing this is a list of URLs");
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

	// NSLog(@"URL list:  %@", imageUrls);

	[pool release];
}

- (void)fetchImage
{
    NSAutoreleasePool *pool=[[NSAutoreleasePool alloc] init];
	currentURLOffset++;
	if(currentURLOffset >= [imageUrls count]) {
		NSLog(@"Resetting URL offset to 0");
		currentURLOffset=0;
	}
	// NSLog(@"URL offset is %d", currentURLOffset);

    NSURL *u=[[NSURL alloc]
		initWithString: [imageUrls objectAtIndex: currentURLOffset]];
    NSURLRequest *theRequest=[NSURLRequest requestWithURL:u
                        cachePolicy:NSURLRequestUseProtocolCachePolicy
                    timeoutInterval:60.0];
    NSURLResponse *theResponse=nil;
    NSError *error=nil;

    NSData *data=[NSURLConnection sendSynchronousRequest: theRequest
        returningResponse:&theResponse error: &error];

    if(data == nil) {
        NSLog(@"Error is %@", error);
    }

    if(data != nil) {
        NSImage *tmpImage=[[NSImage alloc] initWithData: data];
        if(tmpImage != nil) {
            [tmpImage setScalesWhenResized: TRUE];
            if(currentImage != nil) {
                // NSLog(@"Releasing old image");
                [currentImage release];
            }
            currentImage=tmpImage;
        }
    } else {
		NSLog(@"Failed to fetch image.");
	}

    [self updateDisplay];
    [u release];
    [pool release];
}

- (void)drawCurrentImage
{
    // NSLog(@"Animating");
    if(currentImage != nil) {
        // Make sure the image has been scaled.
        NSSize imageSize=[currentImage size];
        if(imageSize.width > screenFrame.size.width
                || imageSize.height > screenFrame.size.height) {
            SizeScaler *ss=[[SizeScaler alloc] initWithSize: [currentImage size]];
            imageSize=[ss scaleTo: screenFrame.size];
            [currentImage setSize: imageSize];
        }
        NSPoint p=[self getRandomPoint: screenFrame.size inner: imageSize];
        NSRect theRect=NSMakeRect(0, 0, imageSize.width, imageSize.height);
        [currentImage drawAtPoint: p fromRect: theRect
            operation: NSCompositeSourceOver fraction: opacity];
    }
}

- (void)updateDisplay
{
    if(blankOnDraw) {
        [self setNeedsDisplay: TRUE];
    } else {
        [self drawCurrentImage];
    }
}

- (void)animateOneFrame
{
    [self fetchImage];
    // [self updateDisplay];
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
    [blankField setState: (blankOnDraw?NSOnState:NSOffState)];
    [opaqueField setFloatValue: opacity];
    [self intervalChanged: intervalField];
    [self opacityChanged: opaqueField];
    return sheet;
}

- (IBAction) okButton:(id)sender
{
    NSLog(@"OK hit");
    ScreenSaverDefaults *defaults =
        [ScreenSaverDefaults defaultsForModuleWithName:@"ImageSaver"];

    float tmpF=[intervalField floatValue];
    [self setAnimationTimeInterval: tmpF];
    [defaults setFloat: tmpF forKey:@"interval"];

    blankOnDraw=([blankField state] == NSOnState);
    [defaults setBool:blankOnDraw forKey:@"blankOnDraw"];

    urlString=[urlField stringValue];
    [defaults setObject: urlString forKey:@"url"];

    opacity=[opaqueField floatValue];
    NSLog(@"Opacity is now %0.2f", opacity);
    [defaults setFloat:opacity forKey:@"opacity"];

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
    int secs=[sender intValue];
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
    [updateIntervalLabel setStringValue: label];
    [pool release];
}

- (IBAction)opacityChanged:(id)sender
{
    NSString *label=[[NSString alloc] initWithFormat: @"%0.0f%%", (100.0*[sender floatValue])];
    [opaqueLabel setStringValue: label];
    [label release];
}

@end
