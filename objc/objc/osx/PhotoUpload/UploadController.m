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
    printf("Number of columns now is %d\n", [imgMatrix numberOfColumns]);
    printf("Number of rows is now %d\n", [imgMatrix numberOfRows]);
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
        id s=[scroller horizontalScroller];
        if(s == nil) {
            NSLog(@"No horizontal scroller\n");
        } else {
            NSLog(@"Horizontal scroller:  %@\n", [s description]);
        }
        NSLog(@"New row is %@\n", [array description]);
        NSLog(@"Matrix is %@\n", [imgMatrix description]);
        printf("Number of columns now is %d\n", [imgMatrix numberOfColumns]);
        printf("Number of rows is now %d\n", [imgMatrix numberOfRows]);
        /*
         images=new NSImage[files.count()];
         for(int i=0; i<files.count(); i++) {
             String fn=(String)files.objectAtIndex(i);
             images[i]=new NSImage(fn, true);
         }
         */
    }

}

- (IBAction)stopUpload:(id)sender
{
}

- (IBAction)upload:(id)sender
{
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
