#import "DumpBox.h"

@implementation DumpBox

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

-(BOOL)performDragOperation:(id <NSDraggingInfo>)sender
{
    NSLog(@"Got some shit dragged in.\n");

    NSPasteboard *pboard=[sender draggingPasteboard];
    NSArray *files = [pboard propertyListForType:NSFilenamesPboardType];
    ThumbNailer *nailer=[[ThumbNailer alloc] init];
    NSEnumerator *e=[files objectEnumerator];
    NSString *str=nil;
    while(str=[e nextObject]) {
        NSLog(@"Got %@\n", str);
        [nailer processFile: str];
    }

    [nailer release];
    return (TRUE);
}

@end
