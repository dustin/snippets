#import "Controller.h"

@implementation Controller

-(void)setUnits: (NSString *)to
{
    NSEnumerator *enumerator = [therms objectEnumerator];
    id object;
    while (object = [enumerator nextObject]) {
        if([to isEqualTo: @"f"]) {
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
        NSString *s=[[NSString alloc]
            initWithFormat: @"http://bleu.west.spy.net/therm/Temperature?temp=%@",
            [object name]];
        NSURL *u=[[NSURL alloc] initWithString: s];
        NSString *sr=[[NSString alloc] initWithContentsOfURL: u];
        [object setReading: [sr floatValue]];
        [s release];
        [u release];
        [sr release];
    }
    NSString *s=[[NSString alloc] initWithFormat: @"Last update:  %@",
        [[NSDate date] description]];
    [status setStringValue: s];
    [s release];
}

-(void)awakeFromNib
{
    // Grab an image
    NSURL *u=[[NSURL alloc]
        initWithString: @"http://bleu.west.spy.net/therm/images/therm-c.gif"];
    NSImage *ci=[[NSImage alloc] initByReferencingURL: u];
    [u release];
    u=[[NSURL alloc]
        initWithString: @"http://bleu.west.spy.net/therm/images/therm-f.gif"];
    NSImage *fi=[[NSImage alloc] initByReferencingURL: u];
    [u release];
    // Create the collection of thermometers
    therms=[[NSMutableArray alloc] initWithCapacity: 6];
    [therms retain];
    [backYard setName: @"backyard"];
    [therms addObject: backYard];
    [bedroom setName: @"bedroom"];
    [therms addObject: bedroom];
    [garage setName: @"garage"];
    [therms addObject: garage];
    [guestRoom setName: @"guestroom"];
    [therms addObject: guestRoom];
    [livingRoom setName: @"livingroom"];
    [therms addObject: livingRoom];
    [machineRoom setName: @"newmachineroom"];
    [therms addObject: machineRoom];

    NSEnumerator *enumerator = [therms objectEnumerator];
    id object;
    while (object = [enumerator nextObject]) {
        [object setCImage: ci];
        [object setFImage: fi];
        [object setCelsius];
    }

    bool unitsSet=false;
    // Check the defaults, make sure this is what the user wants
    id defaults=[NSUserDefaults standardUserDefaults];
    id defaultUnit=[defaults objectForKey:@"units"];
    if(defaultUnit != nil) {
        if([defaultUnit isEqualTo: @"f"]) {
            [self setFarenheit: nil];
            unitsSet=true;
        }
    }
    if(unitsSet==false) {
        [self setCelsius: nil];
    }

    [self update];

    // Schedule the timer
    [NSTimer scheduledTimerWithTimeInterval:60
        target: self
        selector: @selector(update)
        userInfo:nil repeats:true];
}

@end
