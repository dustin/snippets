/* BatchUploadController */

#import <Cocoa/Cocoa.h>
#import "PhotoUpload.h"

@interface BatchUploadController : NSWindowController
{
    IBOutlet NSProgressIndicator *batchProgressBar;
    IBOutlet NSTextField *batchText;
    IBOutlet NSProgressIndicator *fileProgressBar;
    IBOutlet NSTextField *fileText;

    // Connection info
    NSString *_url;
    NSString *_username;
    NSString *_password;

    // Batch specific counts
    int _batchSize;
    int _currentBatch;
    // File (image) specific counts
    int _numFiles;
    int _currentFile;
}
- (IBAction)stopUpload:(id)sender;

-(void)setUrl: (NSString *)url;
-(void)setUsername: (NSString *)username;
-(void)setPassword: (NSString *)password;

-(void)processFiles:(NSArray *)files;
-(void)uploadError: (id)msg;
-(void)uploadedFile;
-(void)uploadComplete;

@end
