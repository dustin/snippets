//
//  rawsocket.h
//  Test
//
//  Created by Dustin Sallings on Sat Jan 12 2002.
//  Copyright (c) 2001 __MyCompanyName__. All rights reserved.
//

#import <dString.h>

@interface dSocket : Object {
	@private
	int s;
}

-init;
-(int)connectTo :(dString *)host port:(int)port;

	// Raw stuff
-(int)send :(char *)msg size:(int)msgSize;
-(int)recv :(char *)buf size:(int)bytes;

	// Native stuff
-(int)sendString :(dString *)string;
-(dString *)readLine;

@end
