//
//  Network.m
//  Pong
//
//  Created by Jeena on 27.02.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "GGSNetwork.h"


@implementation GGSNetwork

#define GGS_HOST @"jeena.net";
#define GGS_PORT 9000
#define NO_TIMEOUT -1

#define CONNECT_RESPONSE_TAG 9
#define HELLO_REQUEST_TAG 10
#define HELLO_RESPONSE_TAG 11
#define DEFINE_REQUEST_TAG 12
#define DEFINE_RESPONSE_TAG 13
#define COMMAND_REQUEST_TAG 14
#define COMMAND_RESPONSE_TAG 15
#define ARGS_RESPONSE_TAG 16

@synthesize asyncSocket, delegate, gameToken, currentCommand;

- (id)initWithDelegate:(id<GGSDelegate>)_delegate {
	if (self = [super init]) {
		delegate = _delegate;
		asyncSocket = [[AsyncSocket alloc] initWithDelegate:self];
		
		[asyncSocket connectToHost:@"jeena.net" onPort:9000 error:nil];
		[asyncSocket readDataToLength:36 withTimeout:NO_TIMEOUT tag:CONNECT_RESPONSE_TAG];
	}
	
	return self;
}

- (void)define:(NSString *)sourceCode {
	NSString *body = [NSString stringWithFormat:@"Token: %@\nServer-Command: define\nContent-Length: %i\n\n%@",
						  self.gameToken,
						  [sourceCode length],
						  sourceCode];
	
	[asyncSocket writeData:[body dataUsingEncoding:NSUTF8StringEncoding] withTimeout:NO_TIMEOUT tag:DEFINE_REQUEST_TAG];
	[asyncSocket readDataToData:[@"\n\n" dataUsingEncoding:NSUTF8StringEncoding] withTimeout:NO_TIMEOUT tag:DEFINE_RESPONSE_TAG];
}

- (void)sendCommand:(NSString *)command withArgs:(NSString *)args {
	NSString *body = [NSString stringWithFormat:@"Token: %@\nGame-Command: %@\nContent-Length: %i\n\n%@",
					  self.gameToken,
					  command,
					  [args length]+1,
					  args];

	[asyncSocket writeData:[body dataUsingEncoding:NSUTF8StringEncoding] withTimeout:NO_TIMEOUT tag:COMMAND_REQUEST_TAG];	
	// [asyncSocket readDataToData:[@"\n\n" dataUsingEncoding:NSUTF8StringEncoding] withTimeout:NO_TIMEOUT tag:COMMAND_RESPONSE_TAG];
	[asyncSocket readDataToData:[@"\n" dataUsingEncoding:NSUTF8StringEncoding] withTimeout:NO_TIMEOUT tag:ARGS_RESPONSE_TAG]; // FIXME change to \n\n abd COMMAND_RESPONSE_TAG
}

- (void)onSocket:(AsyncSocket *)sock didConnectToHost:(NSString *)host port:(UInt16)port {
	
}

- (void)onSocket:(AsyncSocket *)sender didReadData:(NSData *)data withTag:(long)tag {
	
	if (tag == CONNECT_RESPONSE_TAG) {
		
		NSString *response = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
		self.gameToken = response;
		[response release];
		
		[delegate GGSNetwork:self ready:YES];
		
	} else if (tag == DEFINE_RESPONSE_TAG) {
		
		[self.delegate GGSNetwork:self defined:YES];
		
	} else if (tag == COMMAND_RESPONSE_TAG) {
		
		NSString *response = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
		NSArray *headers = [response componentsSeparatedByString:@"\n"];
		[response release];

		for (NSInteger i = 0; i < [headers count]; i++) {
			NSString *header = [headers objectAtIndex:i];
			
			if ([header rangeOfString:@"Client-Command: "].location == 0) {
				self.currentCommand = [header substringFromIndex:16];
			} else if ([header rangeOfString:@"Size: "].location == 0) {
				[asyncSocket readDataToLength:[[header substringFromIndex:6] intValue] withTimeout:NO_TIMEOUT tag:ARGS_RESPONSE_TAG];
			}
		}
		
	} else if (tag == ARGS_RESPONSE_TAG) {
		
		NSString *response = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
		[delegate GGSNetwork:self gotCommand:self.currentCommand withArgs:response];
		[response release];
		//self.currentCommand = nil;
		
		[asyncSocket readDataToData:[@"\n" dataUsingEncoding:NSUTF8StringEncoding] withTimeout:NO_TIMEOUT tag:ARGS_RESPONSE_TAG];
		
	}
}

- (void)dealloc {
	[asyncSocket release];
	[gameToken release];
	[currentCommand release];
	
	[super dealloc];
}

@end
