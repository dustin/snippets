#import <Foundation/Foundation.h>
#import "RemoteObject.h"

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

@interface Authenticator : NSObject {
}
@end

@implementation Authenticator
- (BOOL)connection:(NSConnection *)parentConnection
  shouldMakeNewConnection:(NSConnection *)newConnection
{
	// NSLog(@"Connection wants to make a new connection to %@", newConnection);
	struct sockaddr_in saddr;

	id remotePort=[newConnection sendPort];
	NSData *d=[remotePort address];
	[d getBytes:&saddr];
	NSLog(@"Remote address is %s", inet_ntoa(saddr.sin_addr));
	return(YES);
}
@end

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	int thePort=3838;
	NSSocketPort *myPort=[[NSSocketPort alloc] initWithTCPPort:thePort];

    id ro=[[RemoteObject alloc] init];

	Authenticator *auth=[[Authenticator alloc] init];

    // set up remote object
    NSConnection *theConnection=[NSConnection
		connectionWithReceivePort:myPort sendPort:nil];
	[theConnection setDelegate:auth];

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
