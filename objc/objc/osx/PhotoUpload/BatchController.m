#import "BatchController.h"

@implementation BatchController

- (void)alert:(id)title message:(id)msg
{
    NSRunAlertPanel(title, msg, @"OK", nil, nil);
}

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

    // Get the location to save the file.
    NSSavePanel *sp;
    int runResult;
    sp = [NSSavePanel savePanel];
    /* set up new attributes */
    // [sp setAccessoryView:newView];
    [sp setRequiredFileType:@"pbatch"];
    runResult = [sp runModal];
    if (runResult == NSOKButton) {
        NSDate *date=[NSCalendarDate dateWithString: [taken stringValue]
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
        NSString *cat=[category stringValue];

        NSArray *files=[imgMatrix files];
        Batch *batch=[[Batch alloc] init];
        [batch setKeywords: k];
        [batch setDescription: d];
        [batch setCategory: cat];
        [batch setTaken: date];
        [batch setFiles: files];

        // Save it to the selected file.
        BOOL result = [NSKeyedArchiver archiveRootObject:batch
                                              toFile:[sp filename]];
        NSLog(@"Wrote %d\n", result);
        [batch release];
    } else {
        NSLog(@"Not saving.\n");
    }
}

-(void)awakeFromNib
{
    NSLog(@"BatchController awaking from nib.\n");
    [imgMatrix clear];
    [imgMatrix registerForDraggedTypes:[NSArray arrayWithObjects:
        NSFilenamesPboardType, nil]];
    [self dateToToday: nil];

    id defaults=[NSUserDefaults standardUserDefaults];
    id catList=[defaults objectForKey:@"categories"];
    if(catList == nil) {
        catList=[NSArray arrayWithObjects: @"Public", @"Private", nil];
    }
    // Initialize the batch category list from the user defaults
    [category removeAllItems];
    [category addItemsWithObjectValues: catList];
}

@end
