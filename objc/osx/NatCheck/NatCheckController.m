#import "NatCheckController.h"

@implementation NatCheckController

/* Helper C function to provider error popups.  It probably doesn't belong
 * here, but it's quick */
void perrordie(const char *msg) {
	char msgbuf[1024];
	int ssize=0;

	ssize=snprintf(msgbuf, sizeof(msgbuf), "%s: %s", msg, strerror(errno));
	msgbuf[sizeof(msgbuf)]=0x00;
	NSRunAlertPanel(@"Error", [NSString stringWithCString: msgbuf],
		@"Quit", nil, nil);

	exit(1);
}

- (void)clearResults
{
    [natType setStringValue: _str(@"Unknown")];
    [consistentTrans setStringValue: _str(@"Unknown")];
    [unsolicitedFilt setStringValue: _str(@"Unknown")];
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
            [natType setStringValue: _str(@"BNAT")];
        } else {
            [natType setStringValue: _str(@"NAPT")];
        }
        if(res.consistent == 1) {
            [consistentTrans setStringValue: _str(@"CONSISTENT")];
        } else {
            [consistentTrans setStringValue: _str(@"INCONSISTENT")];
        }
        if(res.unsolicitedFilt == 1) {
            [unsolicitedFilt setStringValue: _str(@"UNSOLFILT")];
        } else {
            [unsolicitedFilt setStringValue: _str(@"NOUNSOLFILT")];
        }
    }
}

- (IBAction)go:(id)sender
{
    [goButton setEnabled: false];
    [self clearResults];
    [natType setStringValue: _str(@"Checking...")];

    [self performSelector: @selector(performTest:) withObject: nil afterDelay:0];
}

-(void)awakeFromNib
{
    [self clearResults];
    [goButton setEnabled: false];
    [natType setStringValue: _str(@"Checking...")];
    [self performSelector: @selector(performTest:) withObject: nil afterDelay:0];
}

@end
