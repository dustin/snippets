/* HideableProgressIndicator */

#import <Cocoa/Cocoa.h>

@interface HideableProgressIndicator : NSProgressIndicator
{
    BOOL _hidden;
}
- (BOOL)hidden;
- (void)setHidden: (BOOL)val;
@end
