//
//  FileHelpers.h
//  SPYFoundation
//
//  Created by Dustin Sallings on Wed Feb 18 2004.
//  Copyright (c) 2004 Dustin Sallings. All rights reserved.
//

#import <Foundation/Foundation.h>

/*!
    @header FileHelpers
    @abstract   Various file and path helpers.
*/


@interface FileHelpers : NSObject {

}

/*!
    @method     trashFile:
    @abstract   Send the given path to the trash.
    @discussion Returns TRUE if successful.
*/

-(BOOL)trashFile: (NSString *)deadFileWalking;

/*!
    @method     getAppSupport:
    @abstract   Get the app support directory for the given app name (making sure it exists).
    @discussion Returns the path, or nil if the path cannot be found/created.
*/

-(NSString *)getAppSupport:(NSString *)appName;

@end
