//
//  ThumbNailer.m
//  ThumbNailer
//
//  Created by Dustin Sallings on Tue Oct 08 2002.
//  Copyright (c) 2002 SPY internetworking. All rights reserved.
//

#import "ThumbNailer.h"
#import "IconFamily.h"

@implementation ThumbNailer

-(void)processFile: (NSString *)imgFile
{
    // Get the image
    NSImage *image=[[NSImage alloc] initWithContentsOfFile: imgFile];
    IconFamily *iconFamily=[[IconFamily alloc] initWithThumbnailsOfImage:image];

    // [IconFamily removeCustomIconFromFile:imgFile];
    [iconFamily setAsCustomIconForFile:imgFile];

    [image release];
    [iconFamily release];    
}

-(void)processFiles: (NSArray *)images
    withProgressBar:(NSProgressIndicator *)progressBar
{
    int totalImages=[images count];

    // Set up the progress bar
    [progressBar setMinValue: 0];
    [progressBar setMaxValue: totalImages];
    [progressBar setDoubleValue: 0];

    int i=0;
    for(i=0; i<totalImages; i++) {
        NSString *imgFile=[images objectAtIndex: i];
        NSLog(@"Saving %@", imgFile);

        // Update the progress text
        [progressBar incrementBy: 1];
        [progressBar displayIfNeeded];
        // Update the text
        NSString *string=[[NSString alloc] initWithFormat:@"Thumbnailing %d of %d",
            i+1, totalImages];
        // [processingText setStringValue: string];
        [string release];
        // [processingText displayIfNeeded];

        [self processFile: imgFile];
    }    
}

@end
