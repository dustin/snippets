/* Controller */

#import <Cocoa/Cocoa.h>
#import "LogOutline.h"
// #import "TestController.h"

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
    IBOutlet NSWindow *logWindow;
    IBOutlet NSOutlineView *logList;

    NSMutableArray *therms;
}
- (IBAction)setCelsius:(id)sender;
- (IBAction)setFarenheit:(id)sender;

-(IBAction)update:(id)sender;
-(IBAction)showLog:(id)sender;
@end
