#import "ThermometerView.h"

@implementation ThermometerView

-(void)postInit
{
    lastReadings=[[NSMutableArray alloc] initWithCapacity: 5];
}

- (id)initWithFrame:(NSRect)frameRect
{
    id rv=[super initWithFrame: frameRect];
    [rv postInit];
    return(rv);
}

- (id)initWithCoder:(NSCoder *)decoder
{
    id rv=[super initWithCoder: decoder];
    [rv postInit];
    return(rv);
}

- (void)dealloc {
    [lastReadings release];
    [cImage release];
    [fImage release];
    [name release];
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
    float rv=reading;
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

-(void)setReading:(float)r
{
    // Normal reading update stuff
    if(reading != r) {
        float oldreading=reading;
        reading=r;
        NSLog(@"Updated %@ (%.2f -> %.2f)", [self name], oldreading, reading);
        [self setNeedsDisplay: true];
    }

    // Keep the array small enough.
    while([lastReadings count] >= RING_BUFFER_SIZE) {
        [lastReadings removeLastObject];
    }
    // Add the current reading
    NSNumber *n=[[NSNumber alloc] initWithFloat: r];
    [lastReadings insertObject:n atIndex: 0];
    [n release];

    // Check to see whether we're going up or down
    n=[lastReadings lastObject];
    // tmp is used to find the directional change from the oldest reading
    float tmp=[n floatValue] - r;
    if(tmp<0) {
        changeState=INCREASING;
    } else if(tmp>0) {
        changeState=DECREASING;
    } else {
        changeState=STABLE;
    }
}

-(void)setName: (NSString *)n
{
    name=n;
    [name retain];
}

-(NSString *)name
{
    return(name);
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
    NSString *rv = [NSString stringWithFormat: @"%@ %.2f", name,
        [self reading]];

    return(rv);
}

/* Draw the underling thermometer, then some lines over it */
- (void)drawRect:(NSRect)rect
{
    [super drawRect: rect];

    // [self setReading: (float)((rand()%18000)/100.0)-40];

    [self lockFocus];

    // Draw the reading
    NSRect bounds = [self bounds];
    NSString *readingStr = [[NSString alloc] initWithFormat: @"%.2f", [self reading]];
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
    switch(changeState) {
        case STABLE:
            readingStr=@"";
            break;
        case INCREASING:
            readingStr=@"+";
            break;
        case DECREASING:
            readingStr=@"-";
            break;
    }
    [attribs setObject:[NSFont fontWithName:@"Monaco" size:12]
                forKey:NSFontAttributeName];
    wordSize=[readingStr sizeWithAttributes: attribs];
    p=NSMakePoint( ((bounds.size.width/2) - (wordSize.width/2)),
                            ((bounds.size.height/2) + (wordSize.height - 6)));
    [readingStr drawAtPoint:p withAttributes:attribs];

    // Now tell it to draw the arm.
    [self drawArm: bounds];

    [self unlockFocus];
}

@end
