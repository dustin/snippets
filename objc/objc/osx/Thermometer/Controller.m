#import "Controller.h"

@implementation Controller


-(void)awakeFromNib
{
    // Grab an image
    NSURL *u=[[NSURL alloc]
        initWithString: @"http://bleu.west.spy.net/therm/images/therm-c.gif"];
    NSImage *i=[[NSImage alloc] initByReferencingURL: u];
    [img setImage: i];

    NSSize s;
    s.width=145;
    s.height=145;
    [img setFrameSize: s];
    [img needsDisplay];

    [u release];
}

@end
