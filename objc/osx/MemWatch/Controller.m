#import "Controller.h"

@implementation Controller

-(void)update {
    // NSLog(@"Controller updating.");
    [stats update];

    NSString *statusStr = [[NSString alloc] initWithFormat: @"Current free:  %dM",
        ([stats memFree] / (1024*1024))];
    [status setStringValue: statusStr];
    [statusStr release];
    [plot addDatum: [stats memFree]];

    // See if the timer's changed
    double erval=[[defaults objectForKey: @"frequency"] doubleValue];
    double cur=(double)[updater timeInterval];
    if(erval != cur) {
        NSLog(@"Time has changed from %.2f to %.2f, updating", cur, erval);
        [updater invalidate];
        [self scheduleTimer];
    }
    [srcUrl setStringValue: [defaults objectForKey: @"url"]];
}

-(IBAction)update:(id)sender
{
    [self update];
}

-(IBAction)launchPreferences:(id)sender
{
    // XXX:  This leaks memory every time the preferences panel is launched
    id prefc=[[PreferenceController alloc] initWithWindowNibName: @"Preferences"];
    [prefc startUp: defaults];
    NSLog(@"Initialized Test");
}

-(void)initDefaults
{
    // Default defaults
    NSMutableDictionary *dd=[[NSMutableDictionary alloc] initWithCapacity: 4];
    [dd setObject: @"http://desktop.dsallings.eng.2wire.com:8080/admin/monitor/mem"
        forKey: @"url"];
    NSNumber *n=[[NSNumber alloc] initWithInt: 15];
    [dd setObject: n forKey: @"frequency"];
    [n release];
    defaults=[NSUserDefaults standardUserDefaults];
    // Add the default defaults
    [defaults registerDefaults: dd];
    [dd release];
}

-(void)scheduleTimer
{
    int freq=[[defaults objectForKey: @"frequency"] intValue];
    NSLog(@"Scheduling timer with frequency:  %d", freq);
    updater=[NSTimer scheduledTimerWithTimeInterval:freq
        target: self
        selector: @selector(update)
        userInfo:nil repeats:true];
}

-(IBAction)clear:(id)sender
{
    [plot clear];
}

-(void)awakeFromNib
{
    NSLog(@"Starting MemWatch Controller.");
    // Initialize the defaults
    [self initDefaults];

    stats=[[Stats alloc] initWithDefaults: defaults];

    // Do the initial update
    [self performSelector: @selector(update)
        withObject:nil
        afterDelay:0];

    // Now schedule a recurring update
    [self scheduleTimer];

}

@end
