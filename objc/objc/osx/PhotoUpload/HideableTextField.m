#import "HideableTextField.h"

@implementation HideableTextField

- (BOOL)hidden
{
    return(_hidden);
}
- (void)setHidden: (BOOL)val
{
    _hidden=val;
    [self setNeedsDisplay:YES];
}

- (void)drawRect:(NSRect)aRect
{
    if (![self hidden])
    {
        [super drawRect:aRect];
    }
}

@end
