/* BatchController */

#import <Cocoa/Cocoa.h>
#import "DumpMatrix.h"

@interface BatchController : NSWindowController
{
    IBOutlet NSButton *addButton;
    IBOutlet NSTextField *category;
    IBOutlet NSTextField *description;
    IBOutlet DumpMatrix *imgMatrix;
    IBOutlet NSTextField *keywords;
    IBOutlet NSButton *saveButton;
    IBOutlet NSTextField *taken;
}
- (IBAction)addPhotos:(id)sender;
- (IBAction)dateToToday:(id)sender;
- (IBAction)saveBatch:(id)sender;
@end
