#import "DumpMatrix.h"

@implementation DumpMatrix

- (id)init
{
    [super init];
    _storage=[[NSMutableDictionary alloc] init];
    return(self);
}

-(void)dealloc
{
    [_storage release];
    [super dealloc];
}

- (NSDragOperation)draggingEntered:(id <NSDraggingInfo>)sender
{
    NSDragOperation sourceDragMask = [sender draggingSourceOperationMask];
    if (sourceDragMask & NSDragOperationLink) {
        return NSDragOperationLink;
    } else if (sourceDragMask & NSDragOperationCopy) {
        return NSDragOperationCopy;
    }
    return NSDragOperationNone;
}

-(BOOL)isImage: (NSString *)file
{
    BOOL rv=false;
    NSString *ext=[[file pathExtension] lowercaseString];

    rv|=[ext isEqualToString: @"jpg"];
    rv|=[ext isEqualToString: @"gif"];
    rv|=[ext isEqualToString: @"png"];

    return(rv);
}

-(void)clear
{
    [_storage release];
    _storage=[[NSMutableDictionary alloc] init];
    [self update];
}

-(NSArray *)files
{
    return([_storage allKeys]);
}

-(void)update
{
    int colcount=[_storage count];
    // If there's not at least one thing to show, show three and disable.
    if(colcount < 1) {
        colcount=3;
        [self setEnabled: FALSE];
    }
    while([self numberOfColumns] > colcount) {
        [self removeColumn:0];
    }
    while([self numberOfColumns] < colcount) {
        [self addColumn];
    }
    while([self numberOfRows]>0) {
        [self removeRow: 0];
    }
    // Create some button cells
    NSMutableArray *cells=[[NSMutableArray alloc] initWithCapacity: [_storage count]];
    NSEnumerator *e=[_storage objectEnumerator];
    id object=nil;
    while(object = [e nextObject]) {
        [cells addObject: object];
    }
    [self addRowWithCells: cells];
    [self sizeToCells];
    [self setMode: NSListModeMatrix];
    [self setAllowsEmptySelection: TRUE];
    [self deselectAllCells];
    [cells release];
}

-(void)addFile:(NSString *)filename
{
    NSImage *img=[[NSImage alloc] initByReferencingFile: filename];
    [img setName: filename];
    [img setScalesWhenResized: YES];
    SizeScaler *sc=[[SizeScaler alloc] initWithSize: [img size]];
    [img setSize: [sc scaleTo: [self cellSize]]];
    NSButtonCell *cell=[[NSButtonCell alloc] init];
    [cell setImage: img];

    [_storage setObject: cell forKey:filename];

    [self setEnabled: TRUE];

    [cell release];
    [sc release];
    [img release];
}

-(void)removeFile:(NSString *)filename
{
    [_storage removeObjectForKey: filename];
}

-(NSArray *)getImages: (NSString *)file
{
    NSMutableArray *justFiles=[[NSMutableArray alloc] init];

    // Figure out if it's a directory or a file.
    // If it's a directory, recurse.  If it's a file, just use it.
    BOOL isDir=FALSE;
    NSFileManager *manager = [NSFileManager defaultManager];
    if ([manager fileExistsAtPath:file isDirectory:&isDir]) {
        if (isDir) {
            NSArray *subpaths = [manager subpathsAtPath:file];
            int i=0;
            for(i=0; i<[subpaths count]; i++) {
                NSString *fullpath=[file
                    stringByAppendingPathComponent: [subpaths objectAtIndex: i]];
                if ([manager fileExistsAtPath:file isDirectory:&isDir]) {
                    [justFiles addObject: fullpath];
                }
            }
        } else {
            [justFiles addObject: file];
        }
    }

    NSMutableArray *rv=[[NSMutableArray alloc] init];
    [rv autorelease];

    NSEnumerator *nse=[justFiles objectEnumerator];
    id object=nil;
    while(object = [nse nextObject]) {
        // NSLog(@"--- %@\n", object);
        if([self isImage: object]) {
            [rv addObject: object];
        }
    }
    // pathExtension

    [justFiles release];
    return(rv);
}

-(BOOL)performDragOperation:(id <NSDraggingInfo>)sender
{
    NSPasteboard *pboard=[sender draggingPasteboard];
    NSArray *files = [pboard propertyListForType:NSFilenamesPboardType];
    int i=0;
    for(i=0; i<[files count]; i++) {
        NSString *filename=[files objectAtIndex: i];
        NSArray *subfilenames=[self getImages: filename];
        int j=0;
        for(j=0; j<[subfilenames count]; j++) {
            id subfilename=[subfilenames objectAtIndex: j];
            [self addFile: subfilename];
            // NSLog(@"Subfile %@\n", subfilename);
        }
        // NSLog(@"Got %@\n", filename);
    }
    [self update];
    return (TRUE);
}

@end
