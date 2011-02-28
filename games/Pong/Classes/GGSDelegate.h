//
//  GGSDelegate.h
//  Pong
//
//  Created by Jeena on 27.02.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "GGSNetwork.h"

@class GGSNetwork;

@protocol GGSDelegate

- (void)GGSNetwork:(GGSNetwork *)ggsNetwork ready:(BOOL)ready;
- (void)GGSNetwork:(GGSNetwork *)ggsNetwork gotCommand:(NSString *)command withArgs:(NSString *)args;
- (void)GGSNetwork:(GGSNetwork *)ggsNetwork defined:(BOOL)defined;

@end
