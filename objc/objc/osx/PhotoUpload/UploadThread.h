//
//  UploadThread.h
//  PhotoUpload
//
//  Created by Dustin Sallings on Wed Sep 25 2002.
//  Copyright (c) 2002 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "UploadParams.h"

@interface UploadThread : NSObject {
    NSString *_url;
    NSString *_username;
    NSString *_password;
    NSString *_category;
    NSString *_keywords;
    NSString *_description;
    NSDate *_taken;
    NSArray *_files;

    UploadParams *params;
}

// Mutators for getting our information
-(void)setUrl: (NSString *)url;
-(void)setUsername: (NSString *)username;
-(void)setPassword: (NSString *)password;
-(void)setCategory: (NSString *)category;
-(void)setKeywords: (NSString *)keywords;
-(void)setDescription: (NSString *)description;
-(void)setDateTaken: (NSDate *)taken;
-(void)setFiles: (NSArray *)files;


-(void)run: (id)object;

@end
