#import "NatCheckController.h"

@implementation NatCheckController

- (void)clearResults
{
    [natType setStringValue: @"Unknown"];
    [consistentTrans setStringValue: @"Unknown"];
    [unsolicitedFilt setStringValue: @"Unknown"];
}

- (void)performTest:(id)sender
{
    struct check_result res;


    [progressBar setUsesThreadedAnimation: true];
    [progressBar startAnimation: self];
    res=performNatCheck(5);
    [progressBar stopAnimation: self];
    [goButton setEnabled: true];

    if(res.nat_type == UNKNOWN) {
        [self clearResults];
    } else {
        if(res.nat_type == BASIC) {
            [natType setStringValue: @"Basic NAT (IP address only)"];
        } else {
            [natType setStringValue: @"NAPT (Network address and port translation)"];
        }
        if(res.consistent == 1) {
            [consistentTrans setStringValue:
                @"Consistent translation (good for P2P)"];
        } else {
            [consistentTrans setStringValue:
                @"Inconsistent translation (bad for P2P)"];
        }
        if(res.unsolicitedFilt == 1) {
            [unsolicitedFilt setStringValue:
                @"Unsolicited messages filtered (good for security)"];
        } else {
            [unsolicitedFilt setStringValue:
                @"Unsolicited messages not filtered (bad for security)"];
        }
    }
}

- (IBAction)go:(id)sender
{
    [goButton setEnabled: false];
    [self clearResults];
    [natType setStringValue: @"Calculating..."];

    [self performSelector: @selector(performTest:) withObject: nil afterDelay:0];
}

-(void)awakeFromNib
{
    [self clearResults];
    [goButton setEnabled: false];
    [natType setStringValue: @"Calculating..."];
    [self performSelector: @selector(performTest:) withObject: nil afterDelay:0];
}

@end
