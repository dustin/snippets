#import "UploadController.h"
#import "UploadThread.h"
#import "UploadParams.h"
#import "SizeScaler.h"
#import <EDCommon/EDCommon.h>
#import <XMLRPC/XMLRPC.h>

@interface UploadController (Private)

- (void)setButtonAction: (int)to;

@end

@implementation UploadController

- (void)alert:(id)title message:(id)msg
{
    NSRunAlertPanel(title, msg, @"OK", nil, nil);
}

- (IBAction)authenticate:(id)sender
{
    /* Set the defaults */
    [defaults setObject: [username stringValue] forKey:@"username"];
    [defaults setObject: [url stringValue] forKey:@"url"];

    id connection=nil;
    NSURL *u=[[NSURL alloc] initWithString:[url stringValue]];

    id catList=nil;
    NS_DURING
        connection = [[XRConnection alloc] initWithURL:u];
        NSArray *args=[NSArray arrayWithObjects:
                        [username stringValue], [password stringValue]];
        catList = [connection performRemoteMethod:@"getCategories.getAddable"
                                        withObjects:args];

        /* Populate the categories */
        [categories removeAllItems];
        [categories addItemsWithTitles:catList];

        /* Out with the auth */
        [authWindow orderOut: self];
        /* In with the uploader */
        [self openUploadWindow: self];

    NS_HANDLER
        [self alert:_str(@"Auth Exception")
			message:[localException description]];
    NS_ENDHANDLER

    [defaults setObject: catList forKey:@"categories"];

    [u release];
    if(connection != nil) {
        [connection release];
    }
}

- (IBAction)dateToToday:(id)sender
{
    id kw=[NSApp keyWindow];
    // Check to see if the key window is the window
    // for the other controller, and do the right thing.
    if(_batchController != nil && (kw == [_batchController window])) {
        [_batchController dateToToday:self];
    } else {
        NSDate *today = [NSDate date];
        NSString *datestr=
            [today descriptionWithCalendarFormat:
                            @"%Y/%m/%d" timeZone: nil
                                          locale: nil];
        [dateTaken setStringValue:datestr];
    }
}

- (IBAction)newBatch:(id)sender
{
	// Create a new batch controller if we don't have one.
	if(_batchController == nil) {
	NSLog(@"initializing controller.\n");
	_batchController = [[BatchController alloc] initWithWindowNibName:
		@"PhotoUploadBatch"];
	NSLog(@"Opening window.\n");
	}
    [authWindow orderOut: self];
    [_batchController showWindow:self];
}

- (IBAction)openAuthWindow:(id)sender
{
    [authWindow makeKeyAndOrderFront: self];
}

- (IBAction)openBatch:(id)sender
{
    id filePanel=[NSOpenPanel openPanel];
    [filePanel setAllowsMultipleSelection: FALSE];
    [filePanel setCanChooseDirectories: FALSE];

    id types=[NSArray arrayWithObjects:@"pbatch", nil];
    int rv = [filePanel runModalForTypes:types];

    if (rv == NSOKButton) {
        id batch = [NSKeyedUnarchiver unarchiveObjectWithFile:
            [[filePanel filenames] objectAtIndex: 0]];
        [categories selectItemWithTitle: [batch category]];
        [dateTaken setStringValue:
            [[batch taken] descriptionWithCalendarFormat: @"%Y/%m/%d"
                                                timeZone: nil
                                                  locale: nil ]];
        [description setStringValue: [batch description]];
        [keywords setStringValue: [batch keywords]];

        [imgMatrix clear];
        NSEnumerator *e=[[batch files] objectEnumerator];
        id object;
        while(object = [e nextObject]) {
            [imgMatrix addFile: object];
        }
        [imgMatrix update];
    }
}

- (IBAction)openUploadWindow:(id)sender
{
    [uploadWindow makeKeyAndOrderFront: self];
}

- (IBAction)removeAllFiles:(id)sender
{
    [imgMatrix clear];
}

- (IBAction)removeSelected:(id)sender
{
    NSArray *a=[imgMatrix selectedCells];
    int i=0;
    for(i=0; i<[a count]; i++) {
        [imgMatrix removeFile: [[[a objectAtIndex: i] image] name]];
    }
    [imgMatrix update];
}

- (IBAction)saveBatch:(id)sender
{

}

- (IBAction)selectFiles:(id)sender
{
    id filePanel=[NSOpenPanel openPanel];
    [filePanel setAllowsMultipleSelection: TRUE];
    [filePanel setCanChooseDirectories: FALSE];

    id types=[NSArray arrayWithObjects:@"jpg", @"jpeg", @"JPG",
        @"GIF", @"gif", @"png", @"PNG", nil];
    int rv = [filePanel runModalForTypes:types];

    if (rv == NSOKButton) {
        NSArray *files=[filePanel filenames];
        // This is what's displayed in the image box.
        int i=0;
        for(i=0; i<[files count]; i++) {
            [imgMatrix addFile: [files objectAtIndex: i]];
        }
    }
    [imgMatrix update];
}

- (IBAction)showFiles:(id)sender
{
    NSLog(@"Files:  %@\n", [imgMatrix files]);
}

- (IBAction)showSelectedImages:(id)sender
{
    NSArray *a=[imgMatrix selectedCells];
    int i=0;
    for(i=0; i<[a count]; i++) {
        NSLog(@"Selected image:  %@\n", [[a objectAtIndex: i] image]);
    }
}

- (IBAction)stopUpload:(id)sender
{
    [params setFinished: TRUE];
    [uploadButton setEnabled: FALSE];
}

- (IBAction)upload:(id)sender
{
    NSDate *date=[NSCalendarDate dateWithString: [dateTaken stringValue]
                                 calendarFormat: @"%Y/%m/%d"];
    NSString *k=[keywords stringValue];
    if([k length] == 0) {
        [self alert:_str(@"A.T.NoKeywords")
            message:_str(@"A.B.NoKeywords")];
        return;
    }
    NSString *d=[description stringValue];
    if([d length] == 0) {
        [self alert:_str(@"A.T.NoDescription")
            message:_str(@"A.B.NoDescription")];
        return;
    }
    NSString *cat=[categories titleOfSelectedItem];
    NSString *u=[username stringValue];
    NSString *p=[password stringValue];

	Batch *batch=[[Batch alloc] init];

    NSArray *files=[imgMatrix files];
    [batch setUrl: [url stringValue]];
    [batch setUsername: u];
    [batch setPassword: p];
    [batch setKeywords: k];
    [batch setDescription: d];
    [batch setCategory: cat];
    [batch setTaken: date];
    [batch setFiles: files];

    if(params != nil) {
        [params release];
    }
    params=[[UploadParams alloc] init];

    [params setController: self];
    [params setUploadErrorMethod: @selector(uploadError:)];
    [params setUploadedFileMethod: @selector(uploadedFile)];
    [params setUploadCompleteMethod: @selector(uploadComplete)];

    // UI updates
    // Fix up the progress bar
    [progressBar setMinValue: 0];
    [progressBar setMaxValue: [files count]];
    [progressBar setDoubleValue: 0];
    [progressBar setHidden: FALSE];
    currentFile=1;
    // And the uploading text
    [self updateProgressText];
    [uploadingText setHidden: FALSE];

    UploadThread *ut=[[UploadThread alloc] init];
	[ut setBatch: batch];
    [NSThread detachNewThreadSelector: @selector(run:)
                                         toTarget:ut withObject: params];

    [self setButtonAction: BUTTON_STOP];
    [addFilesButton setEnabled: FALSE];
    [ut release];
}

- (void)updateProgressText
{
    if(currentFile <= [[imgMatrix files] count])
    {
        NSString *msg=[NSString stringWithFormat:_str(@"UploadingText"),
            currentFile, [[imgMatrix files] count]];
        [uploadingText setStringValue: msg];
        [uploadingText displayIfNeeded];
    }
}

- (void)setButtonAction: (int)to
{
    switch(to) {
        case BUTTON_UPLOAD:
            [uploadButton setTitle:_str(@"B.Upload")];
            [uploadButton setAction:@selector(upload:)];
            [uploadButton setToolTip: _str(@"B.Upload.ToolTip")];
            break;
        case BUTTON_STOP:
            [uploadButton setTitle:_str(@"B.Stop")];
            [uploadButton setAction:@selector(stopUpload:)];
            [uploadButton setToolTip: _str(@"B.Stop.ToolTip")];
            break;
    }
    [uploadButton setNeedsDisplay: TRUE];
}

-(void)uploadError: (id)msg
{
    [self alert:_str(@"Upload Error") message: msg];
}

-(void)uploadedFile
{
    // NSLog(@"Uploaded a file.\n");
    currentFile++;
    [self updateProgressText];
    [progressBar incrementBy: 1];
}

-(void)uploadComplete
{
    NSLog(@"Upload is complete.\n");
    [addFilesButton setEnabled: TRUE];
    [uploadingText setHidden: TRUE];
    [progressBar setMinValue: 0];
    [progressBar setMaxValue: 0];
    [progressBar setDoubleValue: 0];
    [progressBar setHidden: TRUE];
    [progressBar displayIfNeeded];
    [self setButtonAction: BUTTON_UPLOAD];
    [uploadButton setEnabled: TRUE];
}

- (void)awakeFromNib
{
    // Set up windows
    [uploadWindow orderOut: self];
    [progressBar setDisplayedWhenStopped: FALSE];
    [progressBar setHidden: TRUE];
    [uploadingText setHidden: TRUE];
    // Initialize the button
    buttonType=BUTTON_UPLOAD;
    [self setButtonAction: buttonType];

    defaults=[NSUserDefaults standardUserDefaults];
    id defaultUrl=[defaults objectForKey:@"url"];
    id defaultUsername=[defaults objectForKey:@"username"];
    if(defaultUrl != nil) {
        [url setStringValue:defaultUrl];
    }
    if(defaultUsername != nil) {
        [username setStringValue:defaultUsername];
    }

    // Fill in form entries with defaults
    [self dateToToday: self];

    [imgMatrix clear];
    [imgMatrix registerForDraggedTypes:[NSArray arrayWithObjects:
        NSFilenamesPboardType, nil]];

    [authWindow makeKeyAndOrderFront: self];
}

@end
