#import <Foundation/Foundation.h>

@protocol RemoteObject
-(int)access;
@end

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	NSSocketPort *myPort=[[NSSocketPort alloc] initRemoteWithTCPPort:3838
		host:@"rubik"];

	NSConnection *myConn=[NSConnection connectionWithReceivePort:nil
		sendPort:myPort];

    id theProxy=[[myConn rootProxy] retain];
	// NSLog(@"Got %@\n", theProxy);
    [theProxy setProtocolForProxy:@protocol(RemoteObject)];

    NSLog(@"Calling remote method\n");
    int rv=[theProxy access];
    NSLog(@"Returned %d\n", rv);

    [pool release];
    return 0;
}
