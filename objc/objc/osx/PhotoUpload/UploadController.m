#import "UploadController.h"
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
        connection = [XRConnection connectionWithURL:[NSURL URLWithString:[url stringValue]]];
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
        /*
        String countString=files.count() + " files selected.";
        fileCount.setStringValue(countString);
         */

        // This is what's displayed in the image box.
        id array=[NSMutableArray arrayWithCapacity: [files count]];
        int i=0;
        for(i=0; i<[files count]; i++) {
            id imgCell=[[NSImageCell alloc] init];
            id img=[[NSImage alloc] initByReferencingFile: [files objectAtIndex: i]];
            [imgCell setImage:img];
            [array addObject: imgCell];
        }
        [self resetMatrix];
        while([imgMatrix numberOfColumns] < [files count]) {
            [imgMatrix addColumn];
        }
        while([imgMatrix numberOfRows]>0) {
            [imgMatrix removeRow: 0];
        }
        [imgMatrix addRowWithCells: array];
        [imgMatrix setEnabled: TRUE];
        NSLog(@"New row is %@\n", [array description]);
        NSLog(@"Matrix is %@\n", [imgMatrix description]);
        NSLog(@"Number of columns now is %d\n", [imgMatrix numberOfColumns]);
        NSLog(@"Number of rows is now %d\n", [imgMatrix numberOfRows]);
    }

}

- (IBAction)stopUpload:(id)sender
{
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

    NSMutableDictionary *dict=[[NSMutableDictionary alloc] initWithCapacity:10];

    [dict setObject:u forKey:@"username"];
    [dict setObject:p forKey:@"password"];
    [dict setObject:k forKey:@"keywords"];
    [dict setObject:d forKey:@"info"];
    [dict setObject:date forKey:@"taken"];
    [dict setObject:cat forKey:@"category"];

    // Fix up the progress bar
    [progressBar setMinValue: 0];
    [progressBar setMaxValue: [files count]];
    [progressBar setDoubleValue: 0];
    [progressBar setHidden: FALSE];
    // And the uploading text
    [uploadingText setHidden: FALSE];
    [uploadButton setEnabled: FALSE];

    int i=0;
    for(i=0; i<[files count]; i++) {
        // Update the progress bar and text
        [progressBar displayIfNeeded];
        [self updateProgressText: i of:[files count]];

        // Get the file data
        NSData *myData = [NSData dataWithContentsOfFile:[files objectAtIndex:i]];
        [dict setObject:myData forKey:@"image"];

        NS_DURING
            // get the XML RPC connection
            connection = [XRConnection connectionWithURL:
                [NSURL URLWithString:[url stringValue]]];
            // Make the call
            NSArray *args=[NSArray arrayWithObject:dict];
            id result = [connection performRemoteMethod:@"addImage.addImage"
                                            withObjects:args];
            NSLog(@"Uploaded image %@\n", result);

        NS_HANDLER
            // On error, open up a window and let the user know
            [self alert:@"Upload Error" message:[localException description]];
        NS_ENDHANDLER
        // Increment the progress bar
        [progressBar incrementBy:1];
    }
    // Hide the progress bar and uploading text
    [progressBar setHidden:TRUE];
    [uploadingText setHidden:TRUE];
    // Indicate the upload window needs to wake up and redraw stuff
    [uploadButton setEnabled: TRUE];

    [dict release];
}

- (void)updateProgressText: (int)current of:(int)max
{
    NSString *msg=[NSString stringWithFormat:@"Uploading %d of %d", (current+1), max];
    [uploadingText setStringValue: msg];
    [uploadingText displayIfNeeded];
}

- (void)awakeFromNib
{
    // Set up windows
    [uploadWindow orderOut: self];
    [progressBar setDisplayedWhenStopped: FALSE];
    [progressBar setHidden: TRUE];
    [uploadingText setHidden: TRUE];
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
