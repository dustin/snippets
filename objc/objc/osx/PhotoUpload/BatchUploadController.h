/* BatchUploadController */

#import <Cocoa/Cocoa.h>

@interface BatchUploadController : NSWindowController
{
    IBOutlet NSProgressIndicator *batchProgressBar;
    IBOutlet NSTextField *batchText;
    IBOutlet NSProgressIndicator *fileProgressBar;
    IBOutlet NSTextField *fileText;
}
- (IBAction)stopUpload:(id)sender;
@end
