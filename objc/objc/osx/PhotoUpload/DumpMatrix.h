/* DumpMatrix */

#import <Cocoa/Cocoa.h>

@interface DumpMatrix : NSMatrix
{
}

- (NSDragOperation)draggingEntered:(id <NSDraggingInfo>)sender;
-(BOOL)performDragOperation:(id <NSDraggingInfo>)sender;

@end
