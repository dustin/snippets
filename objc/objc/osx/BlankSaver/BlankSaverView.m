//
//  BlankSaverView.m
//  BlankSaver
//
//  Created by Dustin Sallings on Thu May 29 2003.
//  Copyright (c) 2003, SPY Internetworking. All rights reserved.
//

#import "BlankSaverView.h"


@implementation BlankSaverView

- (id)initWithFrame:(NSRect)frame isPreview:(BOOL)isPreview
{
    self = [super initWithFrame:frame isPreview:isPreview];
    if (self) {
        [self setAnimationTimeInterval:1];
    }
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
    return;
}

- (BOOL)hasConfigureSheet
{
    return NO;
}

- (NSWindow*)configureSheet
{
    return nil;
}

@end
