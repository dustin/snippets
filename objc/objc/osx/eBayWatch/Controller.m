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
    [table setNeedsDisplay: true];
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

-(void)update
{
    NSLog(@"Updating...");
	[self setBusy: true];
    float t=0.0;
    NSEnumerator *enumerator = [watching objectEnumerator];
    id object;
    while (object = [enumerator nextObject]) {
        [object update];
        t+=[object price];
    }
    [table setNeedsDisplay: true];
    [total setFloatValue: t];
    [total setNeedsDisplay: true];
    [self setBusy: false];
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
}

-(void)awakeInitialization
{
    howBusy=0;
    [busySignal setUsesThreadedAnimation: true];
    [busySignal setHidden: true];
    [busySignal setStyle: NSProgressIndicatorSpinningStyle];

    defaults=[NSUserDefaults standardUserDefaults];
    id savedWatching=[defaults objectForKey:@"watching"];
    [self initSaved: savedWatching];
}

- (void)awakeFromNib
{
    NSLog(@"Initializing");

    watching=[[Watching alloc] init];
    [table setDataSource: watching];
    [total setFloatValue: 0.0];

    // one-off timer to initialize after this window loads.
    [NSTimer scheduledTimerWithTimeInterval:.01
        target: self
        selector: @selector(awakeInitialization)
        userInfo: nil
        repeats: false];

    // Schedule the timer
    [NSTimer scheduledTimerWithTimeInterval:60
        target: self
        selector: @selector(update)
        userInfo:nil repeats:true];

}

@end
