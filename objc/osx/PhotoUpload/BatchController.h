/* BatchController */

#import <Cocoa/Cocoa.h>
#import "DumpMatrix.h"
#import "Batch.h"
#import "PhotoUpload.h"

@interface BatchController : NSWindowController
{
    IBOutlet NSButton *addButton;
    IBOutlet NSComboBox *category;
    IBOutlet NSTextField *description;
    IBOutlet DumpMatrix *imgMatrix;
    IBOutlet NSTextField *keywords;
    IBOutlet NSButton *saveButton;
    IBOutlet NSTextField *taken;
}
- (IBAction)addPhotos:(id)sender;
- (IBAction)dateToToday:(id)sender;
- (IBAction)saveBatch:(id)sender;

- (DumpMatrix *)imgMatrix;
@end
