//
//  UploadParams.m
//  PhotoUpload
//
//  Created by Dustin Sallings on Wed Sep 25 2002.
//  Copyright (c) 2002 SPY internetworking. All rights reserved.
//

#import "UploadParams.h"


@implementation UploadParams

-(BOOL)finished
{
    return(_finished);
}

-(void)setFinished:(BOOL)to
{
    _finished=to;
}


// UI methods
-(void)setController: (id)controller
{
    _controller=controller;
}

-(void)setUploadErrorMethod: (SEL)uploadError
{
    _uploadErrorMethod=uploadError;
}

-(void)setUploadedFileMethod: (SEL)markUploaded
{
    _uploadFileMethod=markUploaded;
}

-(void)setUploadCompleteMethod: (SEL)markComplete
{
    _uploadCompleteMethod=markComplete;
}

// Methods used in the thread to indicate changes.
-(void)uploadedFile
{
    [_controller performSelector: _uploadFileMethod];
}

-(void)uploadComplete
{
    [_controller performSelector: _uploadCompleteMethod];
}

-(void)uploadError: (id)object
{
    [_controller performSelector: _uploadErrorMethod withObject: object];
}

@end
