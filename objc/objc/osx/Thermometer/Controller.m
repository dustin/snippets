#import "Controller.h"

@implementation Controller

-(void)setUnits: (NSString *)to
{
    NSEnumerator *enumerator = [therms objectEnumerator];
    id object;
    while (object = [enumerator nextObject]) {
        if((to!=nil) && [to isEqualTo: @"f"]) {
            [object setFarenheit];
        } else {
            [object setCelsius];
            // In case something weird is passed in.
            to=@"c";
        }
    }
    id defaults=[NSUserDefaults standardUserDefaults];
    [defaults setObject: to forKey: @"units"];
}

- (IBAction)setCelsius:(id)sender
{
    [self setUnits: @"c"];
}

- (IBAction)setFarenheit:(id)sender
{
	[self setUnits: @"f"];
}

-(void)update
{
    NSLog(@"Updating.");
    NSEnumerator *enumerator = [therms objectEnumerator];
    id object;
    while (object = [enumerator nextObject]) {
        [[object therm] update];
        // Update the menu
        [[dockMenu itemWithTag: [object tag]] setTitle: [object description]];
    }
    NSString *s=[[NSString alloc] initWithFormat: @"Last update:  %@",
        [[NSDate date] description]];
    [status setStringValue: s];
    [s release];
}

// Updates from the UI
-(IBAction)update:(id)sender
{
    [self update];
}

-(IBAction)showLog:(id)sender
{
    [logWindow makeKeyAndOrderFront: self];
}

-(void)awakeFromNib
{
    // Load the images from the bundle
    NSBundle *mainBundle = [NSBundle mainBundle];
    NSString *path = [mainBundle pathForResource:@"therm-c" ofType:@"png"];
    NSImage *ci = [[NSImage alloc]initWithContentsOfFile:path];
    path = [mainBundle pathForResource:@"therm-f" ofType:@"png"];
    NSImage *fi = [[NSImage alloc]initWithContentsOfFile:path];

    // Get the test controller.
    NSLog(@"Initializing Test");
    /*
    id testc=[[TestController alloc] initWithWindowNibName: @"Test"];
    [testc startUp];
    NSLog(@"Initialized Test");
    */

    // Create the collection of thermometers
    therms=[[NSMutableArray alloc] initWithCapacity: 6];

    // Initialize all of the individual thermometers
    /*
    Thermometer *t=[[Thermometer alloc] initWithName: @"backyard"];
    [backYard setTherm: t];
    [therms addObject: backYard];
    [t release];
    t=[[Thermometer alloc] initWithName: @"bedroom"];
    [bedroom setTherm: t];
    [therms addObject: bedroom];
    [t release];
    t=[[Thermometer alloc] initWithName: @"garage"];
    [garage setTherm: t];
    [therms addObject: garage];
    [t release];
    t=[[Thermometer alloc] initWithName: @"guestroom"];
    [guestRoom setTherm: t];
    [therms addObject: guestRoom];
    [t release];
    t=[[Thermometer alloc] initWithName: @"livingroom"];
    [livingRoom setTherm: t];
    [therms addObject: livingRoom];
    [t release];
    t=[[Thermometer alloc] initWithName: @"newmachineroom"];
    [machineRoom setTherm: t];
    [therms addObject: machineRoom];
    [t release];
    */

    NSEnumerator *enumerator = [therms objectEnumerator];
    id object;
    int tagI=0;
    while (object = [enumerator nextObject]) {
        [object setCImage: ci];
        [object setFImage: fi];
        [object setCelsius];
        [object setTag: tagI];
        NSMenuItem *mi=[[[NSMenuItem alloc] initWithTitle:[object description]
            action:nil keyEquivalent:@""] autorelease];
        [mi setTag: tagI];
        [dockMenu addItem: mi];
        tagI++;
    }

    // Check the defaults, make sure this is what the user wants
    id defaults=[NSUserDefaults standardUserDefaults];
    id defaultUnit=[defaults objectForKey:@"units"];
    [self setUnits: defaultUnit];
    /*
    [self update];

    // set up us the log view
    LogOutline *lout=[[LogOutline alloc] initWithArray: therms];
    [logList setDataSource: lout];

    // Schedule the timer
    [NSTimer scheduledTimerWithTimeInterval:SAMPLE_RATE
        target: self
        selector: @selector(update)
        userInfo:nil repeats:true];
        */
}

@end
