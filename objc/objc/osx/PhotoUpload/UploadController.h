/* UploadController */

#import <Cocoa/Cocoa.h>
#import <EDCommon/EDCommon.h>
#import <XMLRPC/XMLRPC.h>
#import "HideableTextField.h"
#import "HideableProgressIndicator.h"
#import "UploadParams.h"

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
    IBOutlet NSMatrix *imgMatrix;
    IBOutlet NSTextField *keywords;
    IBOutlet NSSecureTextField *password;
    IBOutlet HideableProgressIndicator *progressBar;
    IBOutlet NSScrollView *scroller;
    IBOutlet NSButton *uploadButton;
    IBOutlet HideableTextField *uploadingText;
    IBOutlet NSWindow *uploadWindow;
    IBOutlet NSTextField *url;
    IBOutlet NSTextField *username;

    NSArray *files;
    NSArray *images;
    NSUserDefaults *defaults;

    UploadParams *params;

    int buttonType;

    int currentFile;
}
- (IBAction)authenticate:(id)sender;
- (IBAction)openAuthWindow:(id)sender;
- (IBAction)openUploadWindow:(id)sender;
- (IBAction)selectFiles:(id)sender;
- (IBAction)stopUpload:(id)sender;
- (IBAction)upload:(id)sender;

- (void)alert:(id)title message:(id)msg;
- (void)updateProgressText;
@end
