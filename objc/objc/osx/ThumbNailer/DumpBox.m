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
    int i=0;
    for(i=0; i<[files count]; i++) {
        NSLog(@"Got %@\n", [files objectAtIndex: i]);
    }

    return (TRUE);
}

@end
