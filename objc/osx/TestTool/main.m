#import <Foundation/Foundation.h>
#import "RemoteObject.h"

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	int thePort=3838;
	NSSocketPort *myPort=[[NSSocketPort alloc] initWithTCPPort:thePort];

    id ro=[[RemoteObject alloc] init];

    // set up remote object
    NSConnection *theConnection=[NSConnection
		connectionWithReceivePort:myPort sendPort:nil];

    // insert code here...
    NSLog(@"Connection is listening on %d", thePort);

    [theConnection setRootObject:ro];
    if ([theConnection registerName:@"RemoteObject"] == NO) {
        NSLog(@"Failed to initialize.\n");
    }

    NSLog(@"Beginning run loop.\n");
    [[NSRunLoop currentRunLoop] run];

    [pool release];
    [ro release];
    return 0;
}
