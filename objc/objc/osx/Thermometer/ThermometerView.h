/* ThermometerView */

#import <Cocoa/Cocoa.h>
#import "Thermometer.h"

@interface ThermometerView : NSImageView
{
    bool celsius;

    NSImage *cImage;
    NSImage *fImage;
    Thermometer *therm;
    bool _showTrend;
}

-(void)setCelsius;
-(void)setFarenheit;
-(void)setCImage: (NSImage *)to;
-(void)setFImage: (NSImage *)to;
-(void)setTherm: (Thermometer *)t;
-(id)therm;

-(void)newReading:(float)r;

// For outline views
- (id)outlineView:(NSOutlineView *)outlineView child:(int)index ofItem:(id)item;
- (BOOL)outlineView:(NSOutlineView *)outlineView isItemExpandable:(id)item;
- (int)outlineView:(NSOutlineView *)outlineView numberOfChildrenOfItem:(id)item;
- (id)outlineView:(NSOutlineView *)outlineView
    objectValueForTableColumn:(NSTableColumn *)tableColumn byItem:(id)item;

@end
