#import "Controller.h"

@implementation Controller

-(void)setBusy: (bool)to
{
    if(to) {
        howBusy++;
        [busySignal startAnimation: self];
        [busySignal setHidden: false];
    } else {
        howBusy--;
        if(howBusy==0) {
            [busySignal stopAnimation: self];
            [busySignal setHidden: true];
        }
    }
}

-(void)addItem: (NSString *)itemId withDescription: (NSString *)str
{
    [self setBusy: true];
    // First, grab a new item.
    Item *i=[[Item alloc]
        initWithId: itemId
        description: str
        price: 0.0];
    // Update it now
    [i update];

    // Update the total
    float t=[i price];
    t+=[total floatValue];
    [total setFloatValue: t];

    // Let's get the total updated
    [total setNeedsDisplay: true];

    // Add it to the watch list.
    [watching addItem: i];
    [i release];
    [table reloadData];
    [addWindow orderOut: self];
    [self setBusy: false];
}

- (IBAction)addItem:(id)sender
{
    NSString *inum=[itemNumber stringValue];
    NSString *desc=[itemDescription stringValue];

	[self addItem: inum withDescription: desc];
    // Update the defaults.
    NSArray *a=[[NSArray alloc] initWithObjects: inum, desc, nil];
    [saved addObject: a];
    [defaults setObject: saved forKey: @"watching"];
    [a release];
}

- (IBAction)openAddWindow:(id)sender
{
    // Bring out the add window.
    [addWindow makeKeyAndOrderFront: self];
}

-(void)setStatus: (NSString *)to
{
    NSLog(@"Setting status to %@", to);
    [status setStringValue: to];
    [status display];
}

-(IBAction)update:(id)sender
{
    NSLog(@"Updating...");
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	[self setBusy: true];
    float t=0.0;
    NSEnumerator *enumerator = [watching objectEnumerator];
    id object;
    while (object = [enumerator nextObject]) {
        [self setStatus: [NSString stringWithFormat: @"Updating: %@",
                                        [object description]]];
        [object update];
        t+=[object price];
    }
    // Update the status and other UI elements.
    [table setNeedsDisplay: true];
    [total setFloatValue: t];
    [total setNeedsDisplay: true];
    [self setBusy: false];
    // Clear the status
    [self setStatus: @""];
    [pool release];
}

-(void)initSaved: (id)savedIn
{
    saved=[[NSMutableArray alloc] initWithCapacity: 10];
    if(savedIn != nil) {
        [saved addObjectsFromArray: savedIn];
    }

    // Now initialize the list
    NSEnumerator *e=[saved objectEnumerator];
    id ob=nil;
    while(ob = [e nextObject]) {
        [self addItem: [ob objectAtIndex: 0]
            withDescription: [ob objectAtIndex: 1]];
    }
	[table reloadData];
}

// this is performed after awakeFromNib...gives us time to get the UI up
// before hanging on an update.
-(void)awakeInitialization
{
    howBusy=0;
    [self setStatus: @"Initializing"];
    // [busySignal setUsesThreadedAnimation: true];
    [busySignal setHidden: true];
    [busySignal setStyle: NSProgressIndicatorSpinningStyle];

    defaults=[NSUserDefaults standardUserDefaults];
    id savedWatching=[defaults objectForKey:@"watching"];
    [self initSaved: savedWatching];
    [self setStatus: @""];
}

- (void)awakeFromNib
{
    NSLog(@"Initializing");

    watching=[table dataSource];
    [total setFloatValue: 0.0];

    // one-off timer to initialize after this window loads.
    [self performSelector: @selector(awakeInitialization)
        withObject:nil
        afterDelay:0];

    // Schedule the timer
    [NSTimer scheduledTimerWithTimeInterval:60
        target: self
        selector: @selector(update:)
        userInfo:nil repeats:true];
}

@end
