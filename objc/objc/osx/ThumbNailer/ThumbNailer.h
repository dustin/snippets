//
//  ThumbNailer.h
//  ThumbNailer
//
//  Created by Dustin Sallings on Tue Oct 08 2002.
//  Copyright (c) 2002 SPY internetworking. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <Cocoa/Cocoa.h>

@interface ThumbNailer : NSObject {

}

-(void)processFile: (NSString *)file;
-(void)processFiles: (NSArray *)fileList withProgressBar:progressBar;

@end
