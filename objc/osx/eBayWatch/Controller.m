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

-(void)updateDefaults
{
	NSMutableArray *allObjs=[[NSMutableArray alloc] initWithCapacity:16];
	NSEnumerator *e=[watching objectEnumerator];
	id object=nil;
	while(object = [e nextObject]) {
		NSArray *a=[[NSArray alloc] initWithObjects:
			[object itemId], [object description], nil];
		[allObjs addObject: a];
		[a release];
	}
	[defaults setObject: allObjs forKey: @"watching"];
	[allObjs release];
}

- (IBAction)addItem:(id)sender
{
    NSString *inum=[itemNumber stringValue];
    NSString *desc=[itemDescription stringValue];

	[self addItem: inum withDescription: desc];
	[self updateDefaults];
}

- (IBAction)openAddWindow:(id)sender
{
    // Bring out the add window.
    [addWindow makeKeyAndOrderFront: self];
}

- (IBAction)removeItem:(id)sender
{
	NSLog(@"We want to delete one:  %d", [table selectedRow]);
	if([table selectedRow] >= 0) {
		[watching removeItem: [table selectedRow]];
		[self updateDefaults];
		[table reloadData];
	}
}

-(void)dataUpdated:(id)notification
{
	NSLog(@"Something was updated");
    // Update the sum and other UI elements.
    float t=0.0;
    NSEnumerator *enumerator = [watching objectEnumerator];
    id object;
    while (object = [enumerator nextObject]) {
        t+=[object price];
    }

    [table reloadData];
    [total setFloatValue: t];
    [total setNeedsDisplay: true];
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
        [object update];
        t+=[object price];
    }
    [self setBusy: false];
    [pool release];
}

-(void)initSaved: (id)savedIn
{
    // Now initialize the list
    NSEnumerator *e=[savedIn objectEnumerator];
    id ob=nil;
    while(ob = [e nextObject]) {
        [self addItem: [ob objectAtIndex: 0]
            withDescription: [ob objectAtIndex: 1]];
    }
}

// this is performed after awakeFromNib...gives us time to get the UI up
// before hanging on an update.
-(void)awakeInitialization
{
    howBusy=0;
    // [busySignal setUsesThreadedAnimation: true];
    [busySignal setHidden: true];
    [busySignal setStyle: NSProgressIndicatorSpinningStyle];

    defaults=[NSUserDefaults standardUserDefaults];
    id savedWatching=[defaults objectForKey:@"watching"];
    [self initSaved: savedWatching];
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

	[[NSNotificationCenter defaultCenter]
		addObserver:self
		selector:@selector(dataUpdated:)
		name:DATA_UPDATED
		object:nil];
}

@end
