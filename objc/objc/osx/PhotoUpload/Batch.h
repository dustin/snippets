//
//  Batch.h
//  PhotoUpload
//
//  Created by Dustin Sallings on Wed Oct 09 2002.
//  Copyright (c) 2002 SPY internetworking. All rights reserved.
//

#import <Foundation/Foundation.h>


@interface Batch : NSObject <NSCoding> {
    NSString *_url;
    NSString *_username;
    NSString *_password;
    NSString *_category;
    NSString *_keywords;
    NSString *_description;
    NSDate *_taken;
    NSArray *_files;
}

-(NSString *)url;
-(void)setUrl:(NSString *)to;

-(NSString *)username;
-(void)setUsername:(NSString *)to;

-(NSString *)password;
-(void)setPassword:(NSString *)to;

-(NSString *)category;
-(void)setCategory:(NSString *)to;

-(NSString *)keywords;
-(void)setKeywords:(NSString *)to;

-(NSString *)description;
-(void)setDescription:(NSString *)to;

-(NSDate *)taken;
-(void)setTaken: (NSDate *)to;

-(NSArray *)files;
-(void)setFiles:(NSArray *)to;

@end
