//
//  Item.m
//  eBayWatch
//
//  Created by Dustin Sallings on Wed Mar 05 2003.
//  Copyright (c) 2003 SPY internetworking. All rights reserved.
//

#import "Item.h"
#import "Controller.h"

@implementation Item

-(id)initWithId: (NSString *)i description: (NSString *)d price:(float)p;
{
    description=d;
    [description retain];
    price=p;
    itemId=i;
    [itemId retain];
    return(self);
}

-(void)setPrice: (float)p
{
    price=p;
}

-(float)price
{
    return(price);
}

-(NSString *)description
{
    return(description);
}

-(NSString *)itemId
{
    return(itemId);
}

-(void)dealloc
{
	NSLog(@"Killing an item:  %@", self);
	[description release];
	[itemId release];
	[super dealloc];
}

-(void)update
{
    NSAutoreleasePool *pool=[[NSAutoreleasePool alloc] init];

    NSString *s=[[NSString alloc]
        initWithFormat: @"http://cgi.ebay.com/ws/eBayISAPI.dll?ViewItem&item=%@&isPrinterFriendly=1",
        itemId];
    NSURL *url=[[NSURL alloc] initWithString: s];
    // NSLog(@"Fetching from %@", url);

	NSURLRequest *theRequest=[NSURLRequest requestWithURL:url
                        cachePolicy:NSURLRequestUseProtocolCachePolicy
                    timeoutInterval:60.0];
	// create the connection with the request
	// and start loading the data
	NSURLConnection *theConnection=[[NSURLConnection alloc]
		initWithRequest:theRequest delegate:self];
	if (theConnection != nil) {
		// Create the NSMutableData that will hold
		// the received data
		responseData=[[NSMutableData data] retain];
	} else {
		NSLog(@"Couldn't make connection for %@", url);
	}

	[s release];
    [url release];
    [pool release];
}

-(void)parseData
{
	NSAutoreleasePool *pool=[[NSAutoreleasePool alloc] init];
	NSString *data=[[NSString alloc] initWithData:responseData encoding:NSASCIIStringEncoding];
    NSRange r=[data rangeOfString: @"Current bid"];
    if(r.location == NSNotFound) {
        NSLog(@"No current bid for %@!", self);
    } else {
        // NSLog(@"Range:  %d for %d from %d bytes", r.location, r.length,
        //		[data length]);
        r.length=256;
        NSString *stmp=[data substringWithRange: r];
        NSArray *a=[stmp componentsSeparatedByString: @"\r\n"];
        NSString *lineFour=[a objectAtIndex: 4];
        // NSLog(@"Line four:  %@\n", lineFour);
        r=[lineFour rangeOfString: @"$"];
        if(r.location == NSNotFound) {
            // NSLog(@"NOT FOUND!");
        } else {
            NSString *endOfFour=[lineFour substringFromIndex: (r.location+1)];
            NSRange r2=[endOfFour rangeOfString: @"<"];
            if(r2.location == NSNotFound) {
                // NSLog(@"NOT FOUND!");
            } else {
                NSString *thePrice=[endOfFour substringToIndex: r2.location];
                // Need to pull out commas
                // NSLog(@"Price input:  %@", thePrice);
                NSMutableString *priceAfter=[[NSMutableString alloc]
                    initWithCapacity: 8];
                [priceAfter appendString: thePrice];
                [priceAfter replaceOccurrencesOfString: @","
                    withString: @"" options: 0
                    range: NSMakeRange(0, [priceAfter length])];
                // Done, float it
                price=[priceAfter floatValue];
                // NSLog(@"Current price:  %.02f", price);
                [priceAfter release];
            }
        }
    }
	[data release];
    [pool release];
}

- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response
{
    // it can be called multiple times, for example in the case of a
    // redirect, so each time we reset the data.
	// NSLog(@"Truncating");
    [responseData setLength:0];
}

- (void)connection:(NSURLConnection *)connection
  didFailWithError:(NSError *)error
{
    // release the connection, and the data object
    [connection release];
    [responseData release];

    // inform the user
    NSLog(@"Connection failed! Error - %@ %@",
          [error localizedDescription],
          [[error userInfo] objectForKey:NSErrorFailingURLStringKey]);
}

- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data
{
	// NSLog(@"Got data");
	[responseData appendData: data];
}

-(void)connectionDidFinishLoading:(NSURLConnection *)connection
{
	// NSLog(@"Response complete");
	[self parseData];

	[[NSNotificationCenter defaultCenter]
		postNotificationName:DATA_UPDATED object:nil];

	[connection release];
    [responseData release];
}

@end
