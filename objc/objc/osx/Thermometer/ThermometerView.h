/* ThermometerView */

#import <Cocoa/Cocoa.h>

#define RING_BUFFER_SIZE 10

typedef enum {STABLE, INCREASING, DECREASING} change_state;

@interface ThermometerView : NSImageView
{
    float reading;
    NSString *name;
    bool celsius;

    NSImage *cImage;
    NSImage *fImage;
    NSMutableArray *lastReadings;
    change_state changeState;
}

-(void)setReading: (float)r;
-(void)setName: (NSString *)n;
-(NSString *)name;
-(void)setCelsius;
-(void)setFarenheit;
-(void)setCImage: (NSImage *)to;
-(void)setFImage: (NSImage *)to;

@end
