/* Controller */

#import <Cocoa/Cocoa.h>
#import <Stats.h>
#import <Plot.h>

@interface Controller : NSWindowController
{
    IBOutlet Plot *plot;
    IBOutlet NSTextField *status;
    IBOutlet NSTextField *srcUrl;

    Stats *stats;
}
@end
