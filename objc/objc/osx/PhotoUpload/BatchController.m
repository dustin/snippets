#import "BatchController.h"

@implementation BatchController

- (IBAction)addPhotos:(id)sender
{
    NSLog(@"addPhotos called.\n");
}

- (IBAction)saveBatch:(id)sender
{
    NSLog(@"saveBatch called.\n");
}

-(void)awakeFromNib
{
    NSLog(@"BatchController awaking from nib.\n");
    [imgMatrix clear];
    [imgMatrix registerForDraggedTypes:[NSArray arrayWithObjects:
        NSFilenamesPboardType, nil]];
}

@end
