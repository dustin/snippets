#import "ThermometerView.h"

@implementation ThermometerView

-(void)setReading:(float)r
{
    reading=r;
}

/* Draw the actual arm of the thermometer here. */
- (void)drawArm:(NSRect)bounds
{
    float x2, y2;
    int trans=-90;
    float rad, angle;
    int over=(bounds.size.width/2)+1;
    int up=(bounds.size.height/2)+1;

    angle=(reading*1.8);
    NSLog(@"Calculated angle:  %.2f", angle);
    angle+=trans;
    rad=((angle/360)* 2 * 3.1415926535897932);
    x2=sin(rad)*39;
    y2=cos(rad)*39;
    x2+=over;
    y2+=up;

    NSPoint p1=NSMakePoint(over, up);
    NSPoint p2=NSMakePoint(x2, y2);
    [NSBezierPath strokeLineFromPoint:p1 toPoint:p2];
}

/* Draw the underling thermometer, then some lines over it */
- (void)drawRect:(NSRect)rect
{
    [super drawRect: rect];

    [self setReading: (float)((rand()%18000)/100.0)-40];

    [self lockFocus];

    // Draw the reading
    NSRect bounds = [self bounds];
    NSString *readingStr = [NSString stringWithFormat: @"%.2f", reading];
    NSMutableDictionary * attribs = [NSMutableDictionary dictionary];

    [attribs setObject:[NSFont fontWithName:@"Times" size:12]
                forKey:NSFontAttributeName];
    [attribs setObject:[NSColor blackColor]
                forKey:NSForegroundColorAttributeName];

    NSSize wordSize=[readingStr sizeWithAttributes: attribs];
    NSLog(@"Text size:  %.0fx%.0f", wordSize.width, wordSize.height);
    NSPoint p=NSMakePoint( ((bounds.size.width/2) - (wordSize.width/2)),
                            ((bounds.size.height/2) - (wordSize.height + 3)));
    [readingStr drawAtPoint:p withAttributes:attribs];

    // Now tell it to draw the arm.
    [self drawArm: bounds];

    [self unlockFocus];
}

@end
