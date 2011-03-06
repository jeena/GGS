//
//  Network.m
//  Pong
//
//  Created by Jeena on 27.02.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "GGSNetwork.h"


@implementation GGSNetwork

#define GGS_HOST @"jeena.net"
#define GGS_PORT 9000
#define NO_TIMEOUT -1

#define HEADER_DELIMITER [@"\n\n" dataUsingEncoding:NSUTF8StringEncoding]

#define NO_TAG 7
#define CONNECT_HEAD 8
#define CONNECT_BODY 9
#define HELLO_HEAD 10
#define HELLO_BODY 11
#define DEFINE_HEAD 12
#define DEFINE_BODY 13
#define COMMAND_HEAD 14
#define COMMAND_BODY 15
#define HEAD 16
#define BODY 17

@synthesize asyncSocket, delegate, gameToken, currentHeaders;

- (id)initWithDelegate:(id<GGSDelegate>)_delegate {
	if (self = [super init]) {
		delegate = _delegate;
		asyncSocket = [[AsyncSocket alloc] initWithDelegate:self];
		
		[asyncSocket connectToHost:GGS_HOST onPort:GGS_PORT error:nil];
		
		[asyncSocket readDataToData:HEADER_DELIMITER withTimeout:NO_TIMEOUT tag:HEAD];
	}
	
	return self;
}

- (NSData *)makeMessageFor:(NSString *)serverOrGame withCommand:(NSString *)command andArgs:(NSString *)args {
	return [[NSString stringWithFormat:@"Token: %@\n%@-Command: %@\nContent-Length: %i\n\n%@",
			self.gameToken,
			serverOrGame,
			command,
			[args length],
			args] dataUsingEncoding:NSUTF8StringEncoding];
}

- (void)define:(NSString *)sourceCode {
	[asyncSocket writeData:[self makeMessageFor:@"Server" withCommand:@"define" andArgs:sourceCode] withTimeout:NO_TIMEOUT tag:NO_TAG];
}

- (void)sendCommand:(NSString *)command withArgs:(NSString *)args {
	[asyncSocket writeData:[self makeMessageFor:@"Game" withCommand:command andArgs:args] withTimeout:NO_TIMEOUT tag:NO_TAG];	
}

- (void)onSocket:(AsyncSocket *)sock didConnectToHost:(NSString *)host port:(UInt16)port {

}

- (void)onSocket:(AsyncSocket *)sender didReadData:(NSData *)data withTag:(long)tag {
	
	if (tag == HEAD) {
		[self parseAndSetHeader:data];

		NSInteger size = [[self.currentHeaders objectForKey:@"Content-Size"] intValue];
		if (size > 0) {
			[asyncSocket readDataToLength:size withTimeout:NO_TIMEOUT tag:BODY];
		} else {
			[delegate GGSNetwork:self receivedCommand:[self.currentHeaders objectForKey:@"Client-Command"] withArgs:@""];
			[asyncSocket readDataToData:HEADER_DELIMITER withTimeout:NO_TIMEOUT tag:HEAD];
		}
		
	} else {
		
		NSString *response = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
		
		NSString *command = [self.currentHeaders objectForKey:@"Client-Command"];
		if ([command isEqualToString:@"defined"]) {
			
			if ([response isEqualToString:@"ok"]) {
				[delegate GGSNetwork:self defined:YES];				
			} else {
				[delegate GGSNetwork:self defined:NO];
			}
			
		} else if ([command isEqualToString:@"hello"]) {
			
			self.gameToken = response;
			
			[delegate GGSNetwork:self ready:YES];
			
		} else {
			[delegate GGSNetwork:self receivedCommand:command withArgs:response];
		}

		[asyncSocket readDataToData:HEADER_DELIMITER withTimeout:NO_TIMEOUT tag:HEAD];		
	}
}

- (void)parseAndSetHeader:(NSData *)headerData {
	NSString *headerString = [[NSString alloc] initWithData:headerData encoding:NSUTF8StringEncoding];
	NSArray *headers = [headerString componentsSeparatedByString:@"\n"];

	NSMutableDictionary *dict = [[NSMutableDictionary alloc] initWithCapacity:[headers count]];
	
	for (NSInteger i=0; i<[headers count]; i++) {
		NSString *header = [headers objectAtIndex:i];
		
		if ([header rangeOfString:@"Client-Command: "].location == 0) {
			[dict setValue:[header substringFromIndex:16] forKey:@"Client-Command"];
		} else if ([header rangeOfString:@"Content-Size: "].location == 0) {
			[dict setValue:[header substringFromIndex:14] forKey:@"Content-Size"];
		}
	}
	
	self.currentHeaders = dict;
	[headerString release];
	[dict release];
} 

- (void)dealloc {
	[asyncSocket release];
	
	[gameToken release];
	[currentHeaders release];
	
	[super dealloc];
}

@end
