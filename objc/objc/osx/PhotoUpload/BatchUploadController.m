#import "BatchUploadController.h"
#import "Batch.h"
#import "UploadParams.h";
#import "Uploadthread.h";

@implementation BatchUploadController

- (void)alert:(id)title message:(id)msg
{
    NSRunAlertPanel(title, msg, @"OK", nil, nil);
}

- (IBAction)stopUpload:(id)sender
{
}

-(void)upload: (Batch *)batch
{
    // Set the current file info
    _numFiles=[[batch files] count];
    _currentFile=1;
    [fileProgressBar setMinValue: 0];
    [fileProgressBar setMaxValue: _numFiles];
    [fileProgressBar setDoubleValue: 0];

    UploadParams *params=[[UploadParams alloc] init];

    [batch setUrl: _url];
    [batch setUsername: _username];
    [batch setPassword: _password];

    [params setController: self];
    [params setUploadErrorMethod: @selector(uploadError:)];
    [params setUploadedFileMethod: @selector(uploadedFile)];
    [params setUploadCompleteMethod: @selector(uploadComplete)];

    UploadThread *ut=[[UploadThread alloc] init];
    [ut setBatch: batch];
    [NSThread detachNewThreadSelector: @selector(run:)
                             toTarget:ut withObject: params];

    [params release];
}

-(void)processFiles:(NSArray *)files
{
    [self showWindow:self];
    _batchSize=[files count];
    _currentBatch=0;
    [batchProgressBar setMinValue: 0];
    [batchProgressBar setMaxValue: _batchSize];
    [batchProgressBar setDoubleValue: 0];

    NSEnumerator *e=[files objectEnumerator];
    id filename=nil;
    while(filename = [e nextObject]) {
        NSLog(@"Processing batch %@\n", filename);

        // Get the batch
        id batch = [NSKeyedUnarchiver unarchiveObjectWithFile:
            filename];

        // Process batch
        [self upload: batch];
        _currentBatch++;
    }
}

- (void)updateProgressText
{
    if(_currentFile <= _numFiles)
    {
        NSString *msg=[NSString stringWithFormat:@"Uploading file %d of %d",
            _currentFile, _numFiles];
        [fileText setStringValue: msg];
        [fileText displayIfNeeded];
    }
}

-(void)uploadError: (id)msg
{
    [self alert:_str(@"Upload Error") message: msg];
}

-(void)uploadedFile
{
    // NSLog(@"Uploaded a file.\n");
    _currentFile++;
    [self updateProgressText];
    [fileProgressBar incrementBy: 1];
    [fileProgressBar displayIfNeeded];
}

-(void)uploadComplete
{
    NSLog(@"Batch upload is complete.\n");
    [fileProgressBar setMinValue: 0];
    [fileProgressBar setMaxValue: 0];
    [fileProgressBar setDoubleValue: 0];
    [fileProgressBar displayIfNeeded];

    // Update the batch progress bar
    [batchProgressBar incrementBy: 1];
    [batchProgressBar displayIfNeeded];
    NSString *msg=[NSString stringWithFormat:@"Uploading batch %d of %d",
        _currentBatch+1, _batchSize];
    [batchText setStringValue: msg];
    [batchText displayIfNeeded];
    [[self window] orderOut: self];
}

-(void)setUrl: (NSString *)url
{
    _url=url;
}

-(void)setUsername: (NSString *)username
{
    _username=username;
}

-(void)setPassword: (NSString *)password
{
    _password=password;
}

@end
