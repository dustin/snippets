//
//  ABFController.m
//  AddressBookFixer
//
//  Created by Dustin Sallings on Thu Nov 13 2003.
//  Copyright (c) 2003 __MyCompanyName__. All rights reserved.
//

#import "ABFController.h"

@implementation ABFController

-(void)awakeFromNib
{
	[progressBar setDisplayedWhenStopped: TRUE];
	[progressBar stopAnimation: self];
	ABAddressBook *ab = [ABAddressBook sharedAddressBook];
	NSLog(@"Looking in %@", ab);
	NSArray *everybody = [ab people];
	[abCount setIntValue: [everybody count]];
	// NSLog(@"Found:  %@", everybody);
}

@end
