//
//  PongViewController.h
//  Pong
//
//  Created by Jeena on 26.01.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "GGSDelegate.h"
#import "GGSNetwork.h"
#import <AVFoundation/AVAudioPlayer.h>

enum GameType {
	kGameTypeSinglePlayer = 0,
	kGameTypeMultiPlayer,
	kGameTypeNetworkMultiPlayer
};

@interface PongViewController : UIViewController <GGSDelegate> {
	IBOutlet UIView *ballView;
	IBOutlet UIView *player1View;
	IBOutlet UIView *player2View;
	IBOutlet UILabel *tapToBegin;
	CGPoint ballVelocity;
	BOOL gamePaused;
	
	IBOutlet UILabel *pointsP1;
	IBOutlet UILabel *pointsP2;
	
	GGSNetwork *ggsNetwork;

	AVAudioPlayer *pingSound;
	AVAudioPlayer *pongSound;
	AVAudioPlayer *lostSound;

}

@property (nonatomic, retain) IBOutlet UIView *ballView;
@property (nonatomic, retain) IBOutlet UIView *player1View;
@property (nonatomic, retain) IBOutlet UIView *player2View;
@property (nonatomic, retain) IBOutlet UIView *tapToBegin;

@property (nonatomic, retain) IBOutlet UILabel *pointsP1;
@property (nonatomic, retain) IBOutlet UILabel *pointsP2;

@property (nonatomic, retain) GGSNetwork *ggsNetwork;

- (void)restart;

- (void)startPositions;
- (void)zeroPoints;

- (void)moveBall;
- (void)positionPlayer:(CGPoint)point;

@end

