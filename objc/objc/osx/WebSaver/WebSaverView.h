//
//  WebSaverView.h
//  WebSaver
//
//  Created by Dustin Sallings on Sun Jul 27 2003.
//  Copyright (c) 2003, SPY internetworking. All rights reserved.
//

#import <ScreenSaver/ScreenSaver.h>
#import <WebKit/WebView.h>
#import <WebKit/WebFrame.h>

@interface WebSaverView : ScreenSaverView
{
    WebView *webview;
    NSString *urlString;
    float updateInterval;

    IBOutlet NSSlider *intervalField;
    IBOutlet NSTextField *updateLabel;
    IBOutlet NSTextField *urlField;
    IBOutlet NSWindow *sheet;
}

- (IBAction)cancelButton:(id)sender;
- (IBAction)intervalChanged:(id)sender;
- (IBAction)okButton:(id)sender;

@end
