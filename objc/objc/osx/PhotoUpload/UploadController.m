#import "UploadController.h"
#import "UploadThread.h"
#import "UploadParams.h"
#import <EDCommon/EDCommon.h>
#import <XMLRPC/XMLRPC.h>

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

    NS_DURING
        id connection = [XRConnection connectionWithURL:[NSURL URLWithString:[url stringValue]]];
        NSArray *args=[NSArray arrayWithObjects: [username stringValue], [password stringValue]];
        id result = [connection performRemoteMethod:@"getCategories.getAddable"
                                        withObjects:args];

        /* Populate the categories */
        [categories removeAllItems];
        [categories addItemsWithTitles:result];

        /* Out with the auth */
        [authWindow orderOut: self];
        /* In with the uploader */
        [self openUploadWindow: self];

    NS_HANDLER
        [self alert:@"Authentication Exception" message:[localException description]];
    NS_ENDHANDLER
}

- (IBAction)openAuthWindow:(id)sender
{
}

- (IBAction)openUploadWindow:(id)sender
{
    [uploadWindow makeKeyAndOrderFront: self];
}

-(void)resetMatrix
{
    int i=0;
    for(i=0; i<[imgMatrix numberOfColumns]; i++) {
        [imgMatrix removeColumn:0];
    }
    if([imgMatrix numberOfRows]>0) {
        [imgMatrix removeRow: 0];
    }
    // [imgMatrix addRow];
    [imgMatrix setEnabled: FALSE];
    /*
     NSLog(@"Number of columns now is %d\n", [imgMatrix numberOfColumns]);
     NSLog(@"Number of rows is now %d\n", [imgMatrix numberOfRows]);
     */
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
        files=[filePanel filenames];

        // This is what's displayed in the image box.
        id array=[NSMutableArray arrayWithCapacity: [files count]];
        int i=0;
        for(i=0; i<[files count]; i++) {
            id f=[files objectAtIndex: i];
            // Get the image, and name it the filename.
            NSImage *img=[[NSImage alloc] initByReferencingFile: f];
            [img setName: f];
            // Get the image cell and set the image and stuff
            NSImageCell *imgCell=[[NSImageCell alloc] init];
            [imgCell setImage:img];
            [imgCell setImageFrameStyle: NSImageFramePhoto];
            // Add it to the array
            [array addObject: imgCell];
            // Don't need these anymore
            [imgCell release];
            [img release];
        }
        [self resetMatrix];
        while([imgMatrix numberOfColumns] < [files count]) {
            [imgMatrix addColumn];
        }
        while([imgMatrix numberOfRows]>0) {
            [imgMatrix removeRow: 0];
        }
        [imgMatrix addRowWithCells: array];
        [imgMatrix sizeToCells];
        [imgMatrix setMode: NSListModeMatrix];
        [imgMatrix selectCellAtRow:0 column:1];
    }

}

- (IBAction)showSelectedImages:(id)sender
{
    NSArray *a=[imgMatrix selectedCells];
    int i=0;
    for(i=0; i<[a count]; i++) {
        NSLog(@"Selected image:  %@\n", [[a objectAtIndex: i] image]);
    }
}

- (void)setButtonAction: (int)to
{
    switch(to) {
        case BUTTON_UPLOAD:
            [uploadButton setTitle:@"Upload"];
            [uploadButton setAction:@selector(upload:)];
            [uploadButton setToolTip: @"Upload selected images."];
            break;
        case BUTTON_STOP:
            [uploadButton setTitle:@"Stop"];
            [uploadButton setAction:@selector(stopUpload:)];
            [uploadButton setToolTip: @"Stop upload after next image completes."];
            break;
    }
    [uploadButton setNeedsDisplay: TRUE];
}

- (IBAction)stopUpload:(id)sender
{
    [params setFinished: TRUE];
    [uploadButton setEnabled: FALSE];
}

-(void)uploadError: (id)msg
{
    [self alert:@"Upload Error" message: msg];
}

-(void)uploadedFile
{
    NSLog(@"Uploaded a file.\n");
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

- (IBAction)upload:(id)sender
{
    NSDate *date=[NSCalendarDate dateWithString: [dateTaken stringValue]
                                 calendarFormat: @"%Y/%m/%d"];
    NSString *k=[keywords stringValue];
    if([k length] == 0) {
        [self alert:@"Keywords not Given"
            message:@"The keywords field must be filled in."];
        return;
    }
    NSString *d=[description stringValue];
    if([d length] == 0) {
        [self alert:@"Description not Given"
            message:@"The description field must be filled in."];
        return;
    }
    NSString *cat=[categories titleOfSelectedItem];
    NSString *u=[username stringValue];
    NSString *p=[password stringValue];

    UploadThread *ut=[[UploadThread alloc] init];

    [ut setUrl: [url stringValue]];
    [ut setUsername: u];
    [ut setPassword: p];
    [ut setKeywords: k];
    [ut setDescription: d];
    [ut setCategory: cat];
    [ut setDateTaken: date];
    [ut setFiles: files];

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

    [NSThread detachNewThreadSelector: @selector(run:)
                                         toTarget:ut withObject: params];

    [self setButtonAction: BUTTON_STOP];
    [addFilesButton setEnabled: FALSE];
    [ut release];
}

- (IBAction)uploadForReal:(id)sender
{
    [self setButtonAction: BUTTON_STOP];

    NSMutableDictionary *dict=[[NSMutableDictionary alloc] initWithCapacity:10];


    // Hide the progress bar and uploading text
    [progressBar setHidden:TRUE];
    [uploadingText setHidden:TRUE];
    // Indicate the upload window needs to wake up and redraw stuff
    [uploadButton setEnabled: TRUE];
    // [self setButtonAction: BUTTON_UPLOAD];

    [dict release];
}

- (void)updateProgressText
{
    if(currentFile <= [files count])
    {
        NSString *msg=[NSString stringWithFormat:@"Uploading %d of %d",
            currentFile, [files count]];
        [uploadingText setStringValue: msg];
        [uploadingText displayIfNeeded];
    }
}

- (void)awakeFromNib
{
    // Set up windows
    [uploadWindow orderOut: self];
    [progressBar setDisplayedWhenStopped: FALSE];
    [progressBar setHidden: TRUE];
    [uploadingText setHidden: TRUE];
    buttonType=BUTTON_UPLOAD;
    [self resetMatrix];

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
    NSDate *today = [NSDate date];
    [dateTaken setStringValue:[today descriptionWithCalendarFormat:@"%Y/%m/%d"
                                                          timeZone:nil locale:nil]];

    [authWindow makeKeyAndOrderFront: self];
}

@end
