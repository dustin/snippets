/* Controller */

#import <Cocoa/Cocoa.h>
#import "Watching.h"
#import "HideableProgressIndicator.h"

@interface Controller : NSObject
{
    IBOutlet NSWindow *addWindow;
    IBOutlet HideableProgressIndicator *busySignal;
    IBOutlet NSTextField *itemDescription;
    IBOutlet NSTextField *itemNumber;
    IBOutlet NSTableView *table;
    IBOutlet NSTextField *total;
    IBOutlet NSTextField *status;

    Watching *watching;
    NSUserDefaults *defaults;
    NSMutableArray *saved;
    int howBusy;
}
- (IBAction)addItem:(id)sender;
- (IBAction)openAddWindow:(id)sender;

-(IBAction)update:(id)sender;
@end
