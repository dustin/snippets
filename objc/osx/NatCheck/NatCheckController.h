/* NatCheckController */

#import <Cocoa/Cocoa.h>

#include <natcheck.h>

#define _str(a) NSLocalizedString(a, nil)

@interface NatCheckController : NSObject
{
    IBOutlet NSTextField *consistentTrans;
    IBOutlet NSButton *goButton;
    IBOutlet NSTextField *natType;
    IBOutlet NSProgressIndicator *progressBar;
    IBOutlet NSTextField *unsolicitedFilt;
}
- (IBAction)go:(id)sender;
@end
