/* Controller */

#import <Cocoa/Cocoa.h>
#import "Watching.h"

#define DATA_UPDATED @"DATA_UPDATED"

@interface Controller : NSObject
{
    IBOutlet NSWindow *addWindow;
    IBOutlet NSTextField *itemDescription;
    IBOutlet NSTextField *itemNumber;
    IBOutlet NSTableView *table;
    IBOutlet NSTextField *total;
    IBOutlet NSTextField *lastChange;
	IBOutlet NSTextField *lastUpdate;

    Watching *watching;
    NSUserDefaults *defaults;
    int howBusy;

	// For sorting
	NSTableColumn *lastColumn;
	NSString *lastColumnName;
	BOOL sortAscending;
}
- (IBAction)addItem:(id)sender;
- (IBAction)importItems:(id)sender;
- (IBAction)exportItems:(id)sender;
- (IBAction)removeItem:(id)sender;
- (IBAction)openAddWindow:(id)sender;

-(IBAction)update:(id)sender;
@end
