/* UploadController */

#import <Cocoa/Cocoa.h>
#import <EDCommon/EDCommon.h>
#import <XMLRPC/XMLRPC.h>
#import "HideableTextField.h"
#import "HideableProgressIndicator.h"
#import "UploadParams.h"
#import "DumpMatrix.h"

#define BUTTON_UPLOAD 1
#define BUTTON_STOP 2

@interface UploadController : NSObject
{
    IBOutlet NSButton *addFilesButton;
    IBOutlet NSWindow *authWindow;
    IBOutlet NSPopUpButton *categories;
    IBOutlet NSTextField *dateTaken;
    IBOutlet NSTextField *description;
    IBOutlet NSTextField *fileCount;
    IBOutlet DumpMatrix *imgMatrix;
    IBOutlet NSTextField *keywords;
    IBOutlet NSSecureTextField *password;
    IBOutlet HideableProgressIndicator *progressBar;
    IBOutlet NSScrollView *scroller;
    IBOutlet NSButton *uploadButton;
    IBOutlet HideableTextField *uploadingText;
    IBOutlet NSWindow *uploadWindow;
    IBOutlet NSTextField *url;
    IBOutlet NSTextField *username;

    NSUserDefaults *defaults;

    UploadParams *params;

    int buttonType;

    int currentFile;
}
- (IBAction)authenticate:(id)sender;
- (IBAction)dateToToday:(id)sender;
- (IBAction)openAuthWindow:(id)sender;
- (IBAction)openUploadWindow:(id)sender;
- (IBAction)removeSelected:(id)sender;
- (IBAction)selectFiles:(id)sender;
- (IBAction)showFiles:(id)sender;
- (IBAction)showSelectedImages:(id)sender;
- (IBAction)stopUpload:(id)sender;
- (IBAction)upload:(id)sender;

- (void)alert:(id)title message:(id)msg;
- (void)updateProgressText;
@end
