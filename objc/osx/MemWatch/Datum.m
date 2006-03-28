#import "Datum.h"

@implementation Datum

-(id)initWithInt: (int)val
{
	[super init];
	_val=val;
	_when=[[NSDate alloc] init];
	return self;
}

-(int)intValue
{
	return(_val);
}

-(NSDate *)when
{
	return(_when);
}

@end
