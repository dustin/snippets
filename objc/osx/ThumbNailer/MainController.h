/* MainController */

#import <Cocoa/Cocoa.h>
#import "DumpBox.h"

@interface MainController : NSObject
{
    IBOutlet NSWindow *dragWindow;
    IBOutlet DumpBox *dumpBox;
    IBOutlet NSWindow *mainWindow;
    IBOutlet NSTextField *processingText;
    IBOutlet NSProgressIndicator *progressBar;
}
- (IBAction)processFiles:(id)sender;
@end
