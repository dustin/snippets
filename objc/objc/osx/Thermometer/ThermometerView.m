#import "ThermometerView.h"

@implementation ThermometerView

-(void)setTherm: (Thermometer *)t
{
    therm=t;
    [therm retain];
    [therm setDelegate: self];
}

-(id)therm
{
    return(therm);
}

- (void)dealloc
{
    [cImage release];
    [fImage release];
    [therm release];
    [super dealloc];
}

float ctof(float c)
{
    float rv=0.0;
    rv=((9.0/5.0)*c) + 32.0;
    return(rv);
}

-(float)reading
{
    float rv=[therm reading];
    if(!celsius) {
        rv=ctof(rv);
    }
    return(rv);
}

-(void)setCelsius
{
    celsius=true;
    [self setImage: cImage];
}

-(void)setFarenheit
{
    celsius=false;
    [self setImage: fImage];
}

-(void)setCImage: (NSImage *)to
{
    [to retain];
    cImage=to;
}

-(void)setFImage: (NSImage *)to
{
    [to retain];
    fImage=to;
}

/* Draw the actual arm of the thermometer here. */
- (void)drawArm:(NSRect)bounds
{
    float x2, y2;
    int trans=-90;
    float rad, angle;
    int over=(bounds.size.width/2)+1;
    int up=(bounds.size.height/2)+1;

    angle=([self reading] * 1.8);
    // NSLog(@"Calculated angle:  %.2f", angle);
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

-(NSString *)description
{
    NSString *rv = [NSString stringWithFormat: @"%@ %.2f", [therm name],
        [self reading]];

    return(rv);
}

//
// Outline view stuff
//

- (id)outlineView:(NSOutlineView *)outlineView child:(int)index ofItem:(id)item
{
    NSLog(@"Asking for child %d of %@", index, item);
    return(nil);
}

- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item
{
    return(false);
}

- (int)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(id)item
{
    NSLog(@"Asking for the number of children of %@", self);
    return(0);
}

// Just get the name of the item
- (id)outlineView:(NSOutlineView *)outlineView
    objectValueForTableColumn:(NSTableColumn *)tableColumn byItem:(id)item
{
    NSLog(@"Getting the item from the view");
    return(item);
}

-(void)newReading:(float)r
{
    NSLog(@"Received delegate notification of new reading:  %.2f", r);
    [self setNeedsDisplay: true];
}

//
// Draw
//

/* Draw the underling thermometer, then some lines over it */
- (void)drawRect:(NSRect)rect
{
    [super drawRect: rect];

    // [self setReading: (float)((rand()%18000)/100.0)-40];

    [self lockFocus];

    // Draw the reading
    NSRect bounds = [self bounds];
    NSString *readingStr = [[NSString alloc] initWithFormat: @"%.2f",
        [self reading]];
    NSMutableDictionary * attribs = [NSMutableDictionary dictionary];

    // Get the text attributes.
    [attribs setObject:[NSFont fontWithName:@"Times" size:12]
                forKey:NSFontAttributeName];
    [attribs setObject:[NSColor blackColor]
                forKey:NSForegroundColorAttributeName];

    // Find out the size of the text, and draw it.
    NSSize wordSize=[readingStr sizeWithAttributes: attribs];
    NSPoint p=NSMakePoint( ((bounds.size.width/2) - (wordSize.width/2)),
                            ((bounds.size.height/2) - (wordSize.height + 3)));
    [readingStr drawAtPoint:p withAttributes:attribs];
    [readingStr release];

    // Draw the change indicator
    NSString *fmt=nil;
    if([therm trend] > 0) {
        fmt=@"+%.2f";
    } else if([therm trend] < 0) {
        fmt=@"%.2f";
    } else {
        fmt=@"";
    }
    readingStr=[[NSString alloc] initWithFormat: fmt, [therm trend]];
    [attribs setObject:[NSFont fontWithName:@"Monaco" size:9]
                forKey:NSFontAttributeName];
    wordSize=[readingStr sizeWithAttributes: attribs];
    p=NSMakePoint( ((bounds.size.width/2) - (wordSize.width/2)),
                            ((bounds.size.height/2) + (wordSize.height - 6)));
    [readingStr drawAtPoint:p withAttributes:attribs];
    [readingStr release];

    // Now tell it to draw the arm.
    [self drawArm: bounds];

    [self unlockFocus];
}

@end
