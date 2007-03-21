#import "MainController.h"
#import "ThumbNailer.h"

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
        // Bring the window
        [mainWindow orderFront: self];

        id nailer=[[ThumbNailer alloc] init];
        [nailer processFiles: images withProgressBar:progressBar];
        
        [mainWindow orderOut: self];
    }
}

-(void)awakeFromNib
{
    // [mainWindow orderOut];
    // [self processFiles: self];
    [dragWindow makeKeyAndOrderFront: self];
    [dumpBox registerForDraggedTypes:[NSArray arrayWithObjects:
        NSFilenamesPboardType, nil]];
}

@end
