//
//  Network.h
//  Pong
//
//  Created by Jeena on 27.02.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "AsyncSocket.h"
#import "GGSDelegate.h"

@protocol GGSDelegate;

@interface GGSNetwork : NSObject {
	AsyncSocket *asyncSocket;
	id<GGSDelegate> delegate;
	NSString *gameToken;
	NSDictionary *currentHeaders;
}

@property (nonatomic, retain) AsyncSocket *asyncSocket;
@property (nonatomic, retain) id<GGSDelegate> delegate;
@property (nonatomic, retain) NSDictionary *currentHeaders;
@property (nonatomic, retain) NSString *gameToken;

- (id)initWithDelegate:(id)delegate;
- (NSData *)makeMessageWithCommand:(NSString *)command andArgs:(NSString *)args;
- (void)parseHeader:(NSData *)headerData;

- (void)define:(NSString *)sourceCode;
- (void)sendCommand:(NSString *)command withArgs:(NSString *)args;

@end
