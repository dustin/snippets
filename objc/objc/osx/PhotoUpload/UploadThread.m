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
    _url=url;
}

-(void)setUsername: (NSString *)username
{
    _username=username;
}

-(void)setPassword: (NSString *)password
{
    _password=password;
}

-(void)setCategory: (NSString *)category
{
    _category=category;
}

-(void)setKeywords: (NSString *)keywords
{
    _keywords=keywords;
}

-(void)setDescription: (NSString *)description
{
    _description=description;
}

-(void)setDateTaken: (NSDate *)taken
{
    _taken=taken;
}

-(void)setFiles: (NSArray *)files
{
    _files=files;
}

-(void)run: (id)object
{
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

    NSMutableDictionary *dict=[[NSMutableDictionary alloc] initWithCapacity:10];

    [dict setObject:_username forKey:@"username"];
    [dict setObject:_password forKey:@"password"];
    [dict setObject:_keywords forKey:@"keywords"];
    [dict setObject:_description forKey:@"info"];
    [dict setObject:_taken forKey:@"taken"];
    [dict setObject:_category forKey:@"category"];

    params=object;
    int i=0;
    for(i=0; i<[_files count] && (! [params finished]); i++) {
        id f=[_files objectAtIndex:i];
        NSLog(@"Uploading %@.", f);

        // Get the file data
        NSData *myData = [NSData dataWithContentsOfFile:f];
        [dict setObject:myData forKey:@"image"];

            NS_DURING
                // get the XML RPC connection
                id connection = [XRConnection connectionWithURL:
                    [NSURL URLWithString:_url]];
                // Make the call
                NSArray *args=[NSArray arrayWithObject:dict];
                id result = [connection performRemoteMethod:@"addImage.addImage"
                                                withObjects:args];
                NSLog(@"Uploaded image %@\n", result);

            NS_HANDLER
                // On error, open up a window and let the user know
                [params uploadError: [localException description]];
            NS_ENDHANDLER
            // Increment the progress bar

        [params uploadedFile];
    }
    NSLog(@"Finished, thread will join.\n");
    [params uploadComplete];

    [pool release];
}

@end
