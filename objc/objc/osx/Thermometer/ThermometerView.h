/* ThermometerView */

#import <Cocoa/Cocoa.h>

@interface ThermometerView : NSImageView
{
    float reading;
}

-(void)setReading: (float)r;

@end
