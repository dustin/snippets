//
//  UploadThread.m
//  PhotoUpload
//
//  Created by Dustin Sallings on Wed Sep 25 2002.
//  Copyright (c) 2002 __MyCompanyName__. All rights reserved.
//

#import "UploadThread.h"
#import <EDCommon/EDCommon.h>
#import <XMLRPC/XMLRPC.h>

@implementation UploadThread

-(void)setBatch:(Batch *)to
{
	id tmp=to;
	_batch=to;
	[_batch retain];
	if(tmp != nil) {
		[tmp release];
	}
}

-(void)run: (id)object
{
	// Create the autorelease pool.
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

	// Retain the parameters
    params=object;
	[params retain];

	// Dictionary for the arguments
    NSMutableDictionary *dict=[[NSMutableDictionary alloc] initWithCapacity:10];

	// The arguments that are always the same
    [dict setObject:[_batch username] forKey:@"username"];
    [dict setObject:[_batch password] forKey:@"password"];
    [dict setObject:[_batch keywords] forKey:@"keywords"];
    [dict setObject:[_batch description] forKey:@"info"];
    [dict setObject:[_batch taken] forKey:@"taken"];
    [dict setObject:[_batch category] forKey:@"category"];

	// URL for uploading
    NSURL *url=[[NSURL alloc] initWithString: [_batch url]];

	NSEnumerator *en=[[_batch files] objectEnumerator];
	id f=nil;
    while( (f = [en nextObject]) && (! [params finished])) {
		// Create an inner autorelease pool to deal with the garbage the
		// upload produces
	NSAutoreleasePool *ipool = [[NSAutoreleasePool alloc] init];

        NSLog(@"Uploading %@.", f);

        // Get the file data
        NSData *myData = [[NSData alloc] initWithContentsOfFile:f];
        [dict setObject:myData forKey:@"image"];

        id connection=nil;

            NS_DURING
                // get the XML RPC connection
                connection = [[XRConnection alloc] initWithURL:url];
                // Make the call
                id result = [connection performRemoteMethod:@"addImage.addImage"
                                                withObject:dict];
                NSLog(@"Uploaded image %@\n", result);

            NS_HANDLER
                // On error, open up a window and let the user know
                [params uploadError: [localException description]];
            NS_ENDHANDLER
            // Increment the progress bar

        if(connection != nil) {
            [connection release];
        }
        [myData release];
        [params uploadedFile];
		[ipool release];
    }
    NSLog(@"Finished, thread will join.\n");
    [params uploadComplete];

    [url release];
    [dict release];
    [pool release];
}

-(void)dealloc
{
	if(_batch!=nil) {
		[_batch release];
		_batch=nil;
	}
	if(params!=nil) {
		[params release];
		params=nil;
	}
	[super dealloc];
}

@end
