/* Controller */

#import <Cocoa/Cocoa.h>
#import "ThermometerView.h"

// How long (in seconds) to sleep between samples
#define SAMPLE_RATE 60

@interface Controller : NSObject
{
    IBOutlet ThermometerView *backYard;
    IBOutlet ThermometerView *bedroom;
    IBOutlet NSMenu *dockMenu;
    IBOutlet ThermometerView *garage;
    IBOutlet ThermometerView *guestRoom;
    IBOutlet ThermometerView *livingRoom;
    IBOutlet ThermometerView *machineRoom;
    IBOutlet NSTextField *status;

    NSMutableArray *therms;
}
- (IBAction)setCelsius:(id)sender;
- (IBAction)setFarenheit:(id)sender;

-(IBAction)update:(id)sender;
@end
