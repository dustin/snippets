//
//  OSXMountainsView.m
//  OSXMountains
//
//  Created by Dustin Sallings on Mon Apr 07 2003.
//  Copyright (c) 2003, SPY internetworking. All rights reserved.
//

#import "OSXMountainsView.h"
#import "AquaGraphics.h"

@implementation OSXMountainsView

Parm fold_param;

-(void)initParameters:(NSRect)frame isPreview:(BOOL)isPreview
{
  fold_param.mean=0;
  fold_param.rg1=FALSE;
  fold_param.rg2=FALSE;
  fold_param.rg3=TRUE;
  fold_param.cross=TRUE;
  fold_param.force_front=TRUE;
  fold_param.force_back=FALSE;
  fold_param.forceval=-1.0;
  fold_param.fdim = 0.65;
  fold_param.mix   =0.0;
  fold_param.midmix=0.0;

}

- (id)initWithFrame:(NSRect)frame isPreview:(BOOL)isPreview
{
    self = [super initWithFrame:frame isPreview:isPreview];
    [self initParameters:frame isPreview:isPreview];
    if (self) {
        [self setAnimationTimeInterval:5.0];
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
	NSLog(@"Drawing in %.0f,%.0f", rect.size.width, rect.size.height);
    [super drawRect:rect];
    NSString *tmpString = @"Test String";
    NSMutableDictionary * attribs = [NSMutableDictionary dictionary];

    // Get the text attributes.
    [attribs setObject:[NSFont fontWithName:@"Times" size:12]
                forKey:NSFontAttributeName];
    [attribs setObject:[NSColor whiteColor]
            forKey:NSForegroundColorAttributeName];
    NSSize wordSize=[tmpString sizeWithAttributes: attribs];
    NSPoint p=NSMakePoint(
        rect.origin.x + ((rect.size.width/2) - (wordSize.width/2)),
        rect.origin.y + (rect.size.height/2));
    [tmpString drawAtPoint:p withAttributes:attribs];
}

- (void)animateOneFrame
{
    [self setNeedsDisplay: true];
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
