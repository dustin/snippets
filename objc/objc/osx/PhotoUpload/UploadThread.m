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

// Set the fields for the upload
-(void)setUrl: (NSString *)url
{
    if(_url != nil) {
	[_url release];
	}
    _url=url;
    [_url retain];
}

-(void)setUsername: (NSString *)username
{
    if(_username != nil) {
	[_username release];
	}
    _username=username;
    [_username retain];
}

-(void)setPassword: (NSString *)password
{
    if(_password != nil) {
	[_password release];
	}
    _password=password;
    [_password retain];
}

-(void)setCategory: (NSString *)category
{
    if(_category != nil) {
	[_category release];
	}
    _category=category;
    [_category retain];
}

-(void)setKeywords: (NSString *)keywords
{
    if(_keywords != nil) {
	[_keywords release];
	}
    _keywords=keywords;
    [_keywords retain];
}

-(void)setDescription: (NSString *)description
{
    if(_description != nil) {
	[_description release];
	}
    _description=description;
    [_description retain];
}

-(void)setDateTaken: (NSDate *)taken
{
    if(_taken != nil) {
	[_taken release];
	}
    _taken=taken;
    [_taken retain];
}

-(void)setFiles: (NSArray *)files
{
    if(_files != nil) {
	[_files release];
	}
    _files=files;
    [_files retain];
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
    [dict setObject:_username forKey:@"username"];
    [dict setObject:_password forKey:@"password"];
    [dict setObject:_keywords forKey:@"keywords"];
    [dict setObject:_description forKey:@"info"];
    [dict setObject:_taken forKey:@"taken"];
    [dict setObject:_category forKey:@"category"];

	// URL for uploading
    NSURL *url=[[NSURL alloc] initWithString: _url];

    int i=0;
    for(i=0; i<[_files count] && (! [params finished]); i++) {
        id f=[_files objectAtIndex:i];
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
    }
    NSLog(@"Finished, thread will join.\n");
    [params uploadComplete];

    [url release];
    [dict release];
    [pool release];
}

-(void)dealloc
{
	if(_url!=nil) {
		[_url release];
		_url=nil;
	}
	if(_username!=nil) {
		[_username release];
		_username=nil;
	}
	if(_password!=nil) {
		[_password release];
		_password=nil;
	}
	if(_category!=nil) {
		[_category release];
		_category=nil;
	}
	if(_keywords!=nil) {
		[_keywords release];
		_keywords=nil;
	}
	if(_description!=nil) {
		[_description release];
		_description=nil;
	}
	if(_taken!=nil) {
		[_taken release];
		_taken=nil;
	}
	if(_files!=nil) {
		[_files release];
		_files=nil;
	}
	if(params!=nil) {
		[params release];
		params=nil;
	}
	[super dealloc];
}

@end
