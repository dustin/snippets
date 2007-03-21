#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>

#import <libgen.h>

int main (int argc, const char * argv[]) {
    int i=0;
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
	NSString *pathPrefix=@"./";

    // insert code here...
    NSWorkspace *ws=[NSWorkspace sharedWorkspace];
    for(i=1; i<argc; i++) {
		int theTag=0;
		char *p=argv[i];
		NSString *theDir=[[NSString alloc] initWithCString: dirname(p)];
		NSString *theFileB=[[NSString alloc] initWithCString: basename(p)];
		NSString *theFile=[pathPrefix stringByAppendingString: theFileB];
		NSArray *files=[[NSArray alloc] initWithObjects: theFile, nil];
        /* NSLog(@"Removing %@ in %@", files, theDir); */
		BOOL result=[ws performFileOperation:NSWorkspaceRecycleOperation
			source: theDir destination:@""
			files: files
			tag: &theTag];
		/* NSLog(@"Returned %d (tag: %d)", result, theTag); */
		[theFileB release];
		[files release];
    }
    
	[pathPrefix release];
    [pool release];
    return 0;
}
