#import <Foundation/Foundation.h>
#import "RemoteObject.h"

int main (int argc, const char * argv[]) {
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

    // insert code here...
    NSLog(@"Hello, World!");

    id ro=[[RemoteObject alloc] init];

    // set up remote object
    NSConnection *theConnection=[NSConnection defaultConnection];
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
