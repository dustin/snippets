#import <Foundation/Foundation.h>

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	id host=nil;

    id theProxy=[NSConnection
		rootProxyForConnectionWithRegisteredName:@"RemoteObject"
		host:host];
	NSLog(@"Got %@\n", theProxy);
	[theProxy retain];
    // [theProxy setProtocolForProxy:@protocol(ServerProtocol)];

    NSLog(@"Calling remote method\n");
    int rv=(int)[theProxy access];
    NSLog(@"Returned %d\n", rv);

    [pool release];
    return 0;
}
