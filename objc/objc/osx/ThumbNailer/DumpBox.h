/* DumpBox */

#import <Cocoa/Cocoa.h>

@interface DumpBox : NSBox
{
}

- (NSDragOperation)draggingEntered:(id <NSDraggingInfo>)sender;
-(BOOL)performDragOperation:(id <NSDraggingInfo>)sender;
@end
