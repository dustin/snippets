#include <sys/sysctl.h>
#import "AVTestController.h"

@implementation AVTestController

int IsAltiVecAvailable( void )
{

    int selectors[2] = { CTL_HW, HW_VECTORUNIT };
    int hasVectorUnit = 0;
    size_t length = sizeof(hasVectorUnit);
    int error = sysctl(selectors, 2, &hasVectorUnit, &length, NULL, 0);

    if( 0 == error ) return hasVectorUnit != 0;

    return 0;

}

- (IBAction)quit:(id)sender
{
    exit(0);
}

-(void)awakeFromNib
{
    if(IsAltiVecAvailable()) {
        [text setStringValue: NSLocalizedString(@"Text.HasAV", @"AV present")];
    } else {
        [text setStringValue: NSLocalizedString(@"Text.HasNotAV", @"No AV present")];
    }
}

@end
