//
//  Batch.m
//  PhotoUpload
//
//  Created by Dustin Sallings on Wed Oct 09 2002.
//  Copyright (c) 2002 SPY internetworking. All rights reserved.
//

#import "Batch.h"


@implementation Batch

-(NSString *)url
{
	return(_url);
}

-(void)setUrl:(NSString *)to
{
	id tmp=_url;
	_url=to;
	[_url retain];
	if(tmp!=nil) {
		[tmp release];
	}
}

-(NSString *)username
{
	return(_username);
}

-(void)setUsername:(NSString *)to
{
	id tmp=_username;
	_username=to;
	[_username retain];
	if(tmp!=nil) {
		[tmp release];
	}
}

-(NSString *)password
{
	return(_password);
}
-(void)setPassword:(NSString *)to
{
	id tmp=_password;
	_password=to;
	[_password retain];
	if(tmp!=nil) {
		[tmp release];
	}
}

-(NSString *)category
{
	return(_category);
}
-(void)setCategory:(NSString *)to
{
	id tmp=_category;
	_category=to;
	[_category retain];
	if(tmp!=nil) {
		[tmp release];
	}
}

-(NSString *)keywords
{
	return(_keywords);
}
-(void)setKeywords:(NSString *)to
{
	id tmp=_keywords;
	_keywords=to;
	[_keywords retain];
	if(tmp!=nil) {
		[tmp release];
	}
}

-(NSString *)description
{
	return(_description);
}
-(void)setDescription:(NSString *)to
{
	id tmp=_description;
	_description=to;
	[_description retain];
	if(tmp!=nil) {
		[tmp release];
	}
}

-(NSDate *)taken
{
	return(_taken);
}
-(void)setTaken: (NSDate *)to
{
	id tmp=_taken;
	_taken=to;
	[_taken retain];
	if(tmp!=nil) {
		[tmp release];
	}
}

-(NSArray *)files
{
	return(_files);
}
-(void)setFiles:(NSArray *)to
{
	id tmp=_files;
	_files=to;
	[_files retain];
	if(tmp!=nil) {
		[tmp release];
	}
}

-(void)dealloc
{
	if(_url!=nil) {
		[_url release];
	}
    if(_username!=nil) {
	[_username release];
	}
    if(_password!=nil) {
	[_password release];
	}
    if(_category!=nil) {
	[_category release];
	}
    if(_keywords!=nil) {
	[_keywords release];
	}
    if(_description!=nil) {
	[_description release];
	}
    if(_taken!=nil) {
	[_taken release];
	}
    if(_files!=nil) {
	[_files release];
	}
	[super dealloc];
}

// Serialization

- (void)encodeWithCoder:(NSCoder *)coder
{
    // [super encodeWithCoder:coder];
    if ( [coder allowsKeyedCoding] ) {
        [coder encodeObject:_url forKey:@"url"];
        [coder encodeObject:_username forKey:@"username"];
        [coder encodeObject:_password forKey:@"password"];
        [coder encodeObject:_category forKey:@"category"];
        [coder encodeObject:_keywords forKey:@"keywords"];
        [coder encodeObject:_description forKey:@"description"];
        [coder encodeObject:_taken forKey:@"taken"];
        [coder encodeObject:_files forKey:@"files"];
    } else {
        [coder encodeObject:_url];
        [coder encodeObject:_username];
        [coder encodeObject:_password];
        [coder encodeObject:_category];
        [coder encodeObject:_keywords];
        [coder encodeObject:_description];
        [coder encodeObject:_taken];
        [coder encodeObject:_files];
    }
    return;
}

- (id)initWithCoder:(NSCoder *)coder
{
    // self = [super initWithCoder:coder];
    if ( [coder allowsKeyedCoding] ) {
        // Can decode keys in any order
        _url=[[coder decodeObjectForKey:@"url"] retain];
        _username=[[coder decodeObjectForKey:@"username"] retain];
        _password=[[coder decodeObjectForKey:@"password"] retain];
        _category=[[coder decodeObjectForKey:@"category"] retain];
        _keywords=[[coder decodeObjectForKey:@"keywords"] retain];
        _description=[[coder decodeObjectForKey:@"description"] retain];
        _taken=[[coder decodeObjectForKey:@"taken"] retain];
        _files=[[coder decodeObjectForKey:@"files"] retain];
    } else {
        // Must decode keys in same order as encodeWithCoder:
        _url=[[coder decodeObject] retain];
        _username=[[coder decodeObject] retain];
        _password=[[coder decodeObject] retain];
        _category=[[coder decodeObject] retain];
        _keywords=[[coder decodeObject] retain];
        _description=[[coder decodeObject] retain];
        _taken=[[coder decodeObject] retain];
        _files=[[coder decodeObject] retain];
    }
    return self;
}

@end
