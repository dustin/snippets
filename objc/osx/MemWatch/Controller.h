/* Controller */

#import <Cocoa/Cocoa.h>
#import <Stats.h>
#import <Plot.h>
#import <PreferenceController.h>

@interface Controller : NSWindowController
{
    IBOutlet Plot *plot;
    IBOutlet NSTextField *status;
    IBOutlet NSTextField *srcUrl;

    NSUserDefaults *defaults;
    NSTimer *updater;
    Stats *stats;
}

-(IBAction)update:(id)sender;
-(IBAction)clear:(id)sender;
-(IBAction)launchPreferences:(id)sender;

-(void)scheduleTimer;

@end
