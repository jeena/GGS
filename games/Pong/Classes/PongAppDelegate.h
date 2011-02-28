//
//  PongAppDelegate.h
//  Pong
//
//  Created by Jeena on 26.01.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>

@class PongViewController;

@interface PongAppDelegate : NSObject <UIApplicationDelegate> {
    UIWindow *window;
    PongViewController *viewController;
}

@property (nonatomic, retain) IBOutlet UIWindow *window;
@property (nonatomic, retain) IBOutlet PongViewController *viewController;

@end

