#import "Controller.h"

@implementation Controller

-(void)update {
    // NSLog(@"Controller updating.");
    [stats update];

    NSString *statusStr = [[NSString alloc] initWithFormat: @"Current free:  %dM",
        ([stats memFree] / (1024*1024))];
    [status setStringValue: statusStr];
    [statusStr release];
    [plot addDatum: [stats memFree]];
}

-(void)awakeFromNib
{
    NSLog(@"Starting MemWatch Controller.");
    NSString *srcUrlStr=@"http://desktop.dsallings.eng.2wire.com:8080/admin/monitor/mem";
    NSURL *memurl=[[NSURL alloc] initWithString: srcUrlStr];
    [srcUrl setStringValue: srcUrlStr];

    stats=[[Stats alloc] initWithUrl: memurl];

    // Do the initial update
    [self performSelector: @selector(update)
        withObject:nil
        afterDelay:0];

    // Now schedule a recurring update
    [NSTimer scheduledTimerWithTimeInterval:15.0
        target: self
        selector: @selector(update)
        userInfo:nil repeats:true];

}

@end
