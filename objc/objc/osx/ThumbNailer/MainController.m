#import "MainController.h"
#import "IconFamily.h"

@implementation MainController

- (IBAction)processFiles:(id)sender
{
    NSArray* imageFileTypes = [NSImage imageFileTypes];

    id filePanel=[NSOpenPanel openPanel];
    [filePanel makeKeyAndOrderFront: self];
    [filePanel setAllowsMultipleSelection: TRUE];
    [filePanel setCanChooseDirectories: FALSE];

    int rv = [filePanel runModalForTypes:imageFileTypes];

    if (rv == NSOKButton && [[filePanel filenames] count] > 0) {
        NSArray *images=[filePanel filenames];
        int totalImages=[images count];

        // Set up the progress bar
        [progressBar setMinValue: 0];
        [progressBar setMaxValue: totalImages];
        [progressBar setDoubleValue: 0];
        // Bring the window
        [mainWindow orderFront: self];

        int i=0;
        for(i=0; i<totalImages; i++) {
            NSString *imgFile=[images objectAtIndex: i];
            NSLog(@"Saving %@", imgFile);

            // Update the progress text
            [progressBar incrementBy: 1];
            [progressBar displayIfNeeded];
            // Update the text
            NSString *string=[[NSString alloc] initWithFormat:@"Thumbnailing %d of %d",
                    i+1, totalImages];
            [processingText setStringValue: string];
            [string release];
            [processingText displayIfNeeded];


            // Get the image
            NSImage *image=[[NSImage alloc] initWithContentsOfFile: imgFile];
            IconFamily *iconFamily=[[IconFamily alloc] initWithThumbnailsOfImage:image];

            // [IconFamily removeCustomIconFromFile:imgFile];
            [iconFamily setAsCustomIconForFile:imgFile];

            [image release];
            [iconFamily release];
        }

        [mainWindow orderOut: self];
    }
}

-(void)awakeFromNib
{
    // [mainWindow orderOut];
    [self processFiles: self];
}

@end
