#import "BatchController.h"

@implementation BatchController

- (IBAction)addPhotos:(id)sender
{
    NSLog(@"addPhotos called.\n");
}

- (IBAction)dateToToday:(id)sender
{
    NSDate *today = [NSDate date];
    NSString *datestr=
        [today descriptionWithCalendarFormat:
                        @"%Y/%m/%d" timeZone: nil
                                      locale: nil];

    [taken setStringValue:datestr];
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
    [self dateToToday: nil];
}

@end
