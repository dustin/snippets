/* ThermometerView */

#import <Cocoa/Cocoa.h>

@interface ThermometerView : NSImageView
{
    float reading;
    NSString *name;
    bool celsius;

    NSImage *cImage;
    NSImage *fImage;
}

-(void)setReading: (float)r;
-(void)setName: (NSString *)n;
-(NSString *)name;
-(void)setCelsius;
-(void)setFarenheit;
-(void)setCImage: (NSImage *)to;
-(void)setFImage: (NSImage *)to;

@end
