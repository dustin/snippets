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
	[myConn setRequestTimeout: 5];
	[myConn setReplyTimeout: 5];

    id theProxy=nil;
	NS_DURING
		theProxy=[[myConn rootProxy] retain];
		// NSLog(@"Got %@\n", theProxy);
    	[theProxy setProtocolForProxy:@protocol(RemoteObject)];
	NS_HANDLER
		NSLog(@"Error getting proxy from connection:  %@",
			[localException description]);
		theProxy=nil;
	NS_ENDHANDLER

    NSLog(@"Calling remote method\n");
    int rv=[theProxy access];
    NSLog(@"Returned %d\n", rv);
    
    [pool release];
    return 0;
}
