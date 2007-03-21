//
//  FileHelpers.m
//  SPYFoundation
//
//  Created by Dustin Sallings on Wed Feb 18 2004.
//  Copyright (c) 2004 Dustin Sallings. All rights reserved.
//

#import "FileHelpers.h"
#import <libgen.h>

@implementation FileHelpers

-(BOOL)trashFile: (NSString *)deadFileWalking
{
	BOOL rv=FALSE;
	
    NSWorkspace *ws=[NSWorkspace sharedWorkspace];
    const char *fn=[deadFileWalking UTF8String];
    NSString *theDir=[[NSString alloc] initWithCString: dirname(fn)];
    NSString *theFile=[[NSString alloc] initWithCString: basename(fn)];
    NSArray *files=[[NSArray alloc] initWithObjects: theFile, nil];
    int theTag=0;
    rv=[ws performFileOperation:NSWorkspaceRecycleOperation
        source: theDir destination:@""
        files: files
        tag: &theTag];
    NSLog(@"Trashing the file returned %d", rv);
    [theDir release];
    [theFile release];
    [files release];
	return(rv);
}

-(NSString *)getAppSupport:(NSString *)appName
{
    BOOL isdir=NO;
    NSString *appsup = [[[NSHomeDirectory()
        stringByAppendingPathComponent:@"Library"]
        stringByAppendingPathComponent:@"Application Support"]
        stringByAppendingPathComponent:appName];
    NSFileManager *manager=[NSFileManager defaultManager];
    if([manager fileExistsAtPath: appsup isDirectory: &isdir]) {
        if(isdir == NO) {
            [self trashFile: appsup];
        }
    } else {
        isdir=NO;
    }
    if(isdir == NO) {
        NSDictionary *attrs=[[NSDictionary alloc] init];
        BOOL rc=[manager createDirectoryAtPath: appsup attributes: attrs];
		if(rc != YES) {
			NSLog(@"Could not create directory at path %@", appsup);
			appsup=nil;
		}
        [attrs release];
    }
    return(appsup);
}


@end
