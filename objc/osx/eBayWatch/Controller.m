#import "Controller.h"

@implementation Controller

-(void)addItem: (NSString *)itemId withDescription: (NSString *)str
{
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
}

-(void)updateDefaults
{
	NSMutableArray *allObjs=[[NSMutableArray alloc] initWithCapacity:16];
	NSEnumerator *e=[watching objectEnumerator];
	id object=nil;
	while(object = [e nextObject]) {
		NSArray *a=[[NSArray alloc]
			initWithObjects: [object itemId], [object description], nil];
		[allObjs addObject: a];
		[a release];
	}
	[defaults setObject: allObjs forKey: @"watching"];
	NSLog(@"Updated defaults with %d items", [allObjs count]);
	[allObjs release];
}

- (IBAction)addItem:(id)sender
{
    NSString *inum=[itemNumber stringValue];
    NSString *desc=[itemDescription stringValue];

	[self addItem: inum withDescription: desc];
	[self updateDefaults];
}

- (IBAction)importItems:(id)sender
{
	id filePanel=[NSOpenPanel openPanel];
	[filePanel setAllowsMultipleSelection: FALSE];
	[filePanel setCanChooseDirectories: FALSE];
	id types=[NSArray arrayWithObjects:@"txt", nil];
	int rv = [filePanel runModalForTypes:types];
	if(rv == NSOKButton) {
		NSString *filename=[[filePanel filenames] objectAtIndex:0];
		NSString *contents=[NSString stringWithContentsOfFile: filename];
		if(contents) {
			NSArray *items=[contents componentsSeparatedByString:@"\n"];
			id e=[items objectEnumerator];
			id object;
			while(object = [e nextObject]) {
				NSArray *parts=[object componentsSeparatedByString:@"\t"];
				if([parts count] == 2) {
					[self addItem:[parts objectAtIndex:0]
						withDescription:[parts objectAtIndex:1]];
				}
			}
		}
	}
	[self updateDefaults];
}

- (IBAction)exportItems:(id)sender
{
	id sp=[NSSavePanel savePanel];
	[sp setRequiredFileType:@"txt"];
	if([sp runModal] == NSOKButton) {
		NSMutableString *s=[[NSMutableString alloc]
			initWithCapacity: 8192];
		NSEnumerator *e=[watching objectEnumerator];
		id object=nil;
		while(object = [e nextObject]) {
			[s appendFormat:@"%@\t%@\n", [object itemId], [object description]];
		}
		if(![s writeToFile:[sp filename] atomically:YES]) {
			NSRunAlertPanel(@"Failed to Save",
				[NSString stringWithFormat:@"Unable to save %@", [sp filename]],
				@"OK", nil, nil);
		}
		[s release];
	}
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

// This is used by the next two methods to produce a date string
-(NSString *)friendlyNow
{
	id dateFormatter = [[NSDateFormatter alloc]
		initWithDateFormat:@"%Y-%m-%d %H:%M"
		allowNaturalLanguage:YES];
	id now=[[NSDate alloc] init];
	NSString *rv=[dateFormatter stringForObjectValue:now];
	[dateFormatter release];
	[now release];
	return(rv);
}

-(void)dataUpdated:(id)notification
{
	// NSLog(@"Something was updated");
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

	// Object of the notification:
	id ob=[notification object];
	NSString *msg=[[NSString alloc]
		initWithFormat: @"Latest change:  %@: %@ -> $%0.2f",
			[self friendlyNow], ob, [ob price]];
	[lastChange setStringValue: msg];
	[msg release];
}

-(IBAction)update:(id)sender
{
    NSLog(@"Updating...");
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    float t=0.0;
    NSEnumerator *enumerator = [watching objectEnumerator];
    id object;
    while (object = [enumerator nextObject]) {
        [object update];
        t+=[object price];
    }

	NSString *msg=[[NSString alloc]
		initWithFormat: @"Latest update:  %@", [self friendlyNow]];
	[lastUpdate setStringValue: msg];
	[msg release];

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
    defaults=[NSUserDefaults standardUserDefaults];
    id savedWatching=[defaults objectForKey:@"watching"];
    [self initSaved: savedWatching];
}

-(void)sortBy:(NSString *)columnName ascending:(BOOL)asc
{
	id priceDescriptor=[[[NSSortDescriptor alloc]
		initWithKey:columnName ascending:asc] autorelease];
	id sortDescriptors=[NSArray arrayWithObject:priceDescriptor];
	[table setSortDescriptors: sortDescriptors];
	[table reloadData];
}

-(void)sortBy:(NSString *)columnName
{
	if([columnName isEqualTo:lastColumnName]) {
		sortAscending = !sortAscending;
	} else {
		[lastColumnName release];
		lastColumnName=columnName;
		[lastColumnName retain];
		sortAscending=YES;
	}
	[self sortBy:lastColumnName ascending:sortAscending];
}

// This is for handling the sorting...
-(void)tableView:(NSTableView *)tableView didClickTableColumn:(NSTableColumn *)col
{
	NSLog(@"Clicked on column %@", [col identifier]);
	[self sortBy:[col identifier]];
	[tableView setIndicatorImage:nil inTableColumn:lastColumn];
	[tableView setIndicatorImage:[NSImage imageNamed:
			(sortAscending?
				@"NSAscendingSortIndicator":@"NSDescendingSortIndicator")]
		inTableColumn:col];
	lastColumn=col;
}

- (void)awakeFromNib
{
    NSLog(@"Initializing");

    watching=[table dataSource];
    [total setFloatValue: 0.0];

	// View sorting
	lastColumnName=[[NSString alloc] initWithString:@"price"];
	sortAscending=NO;
	[self sortBy:lastColumnName ascending:sortAscending];

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
