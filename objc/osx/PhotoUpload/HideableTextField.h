/* HideableTextField */

#import <Cocoa/Cocoa.h>

@interface HideableTextField : NSTextField
{
    BOOL _hidden;
}
- (BOOL)hidden;
- (void)setHidden: (BOOL)val;
@end
