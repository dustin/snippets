/* ThermometerView */

#import <Cocoa/Cocoa.h>

#define RING_BUFFER_SIZE 10

@interface ThermometerView : NSImageView
{
    float reading;
    NSString *name;
    bool celsius;

    NSImage *cImage;
    NSImage *fImage;
    NSMutableArray *lastReadings;
    float trend;
    bool _showTrend;
}

-(void)setReading: (float)r;
-(void)setName: (NSString *)n;
-(NSString *)name;
-(void)setCelsius;
-(void)setFarenheit;
-(void)setCImage: (NSImage *)to;
-(void)setFImage: (NSImage *)to;

-(NSArray *)lastReadings;

-(void)update;

// For outline views
- (id)outlineView:(NSOutlineView *)outlineView child:(int)index ofItem:(id)item;
- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item;
- (int)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(id)item;
- (id)outlineView:(NSOutlineView *)outlineView
    objectValueForTableColumn:(NSTableColumn *)tableColumn byItem:(id)item;

@end
