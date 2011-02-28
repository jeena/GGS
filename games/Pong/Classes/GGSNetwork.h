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
	NSString *currentCommand;
}

@property (nonatomic, retain) AsyncSocket *asyncSocket;
@property (nonatomic, retain) id<GGSDelegate> delegate;
@property (nonatomic, retain) NSString *gameToken;
@property (nonatomic, retain) NSString *currentCommand;

- (id)initWithDelegate:(id)delegate;
- (void)define:(NSString *)sourceCode;
- (void)sendCommand:(NSString *)command withArgs:(NSString *)args;

@end
