/* MainController */

#import <Cocoa/Cocoa.h>

@interface MainController : NSObject
{
    IBOutlet NSWindow *mainWindow;
    IBOutlet NSTextField *processingText;
    IBOutlet NSProgressIndicator *progressBar;
}
- (IBAction)processFiles:(id)sender;
@end
