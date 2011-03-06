//
//  PongViewController.m
//  Pong
//
//  Created by Jeena on 26.01.11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "PongViewController.h"

@implementation PongViewController

#define PLAYER_SPEED 20
#define BALL_SPEED_X 7
#define BALL_SPEED_Y 5
#define INTERVAL 0.05
#define WIDTH 480
#define HEIGHT 320

#define TOX(x) ( 4.8 * x )
#define TOY(y) ( 3.2 * y )

@synthesize ballView, player1View, player2View, tapToBegin, pointsP1, pointsP2, ggsNetwork;

/*
// The designated initializer. Override to perform setup that is required before the view is loaded.
- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}
*/

/*
// Implement loadView to create a view hierarchy programmatically, without using a nib.
- (void)loadView {
	[super loadView];
}
*/

-(BOOL)canBecomeFirstResponder {
    return YES;
}

#pragma mark -
#pragma mark GGSNetwork Delegate

- (void)GGSNetwork:(GGSNetwork *)_ggsNetwork ready:(BOOL)ready {
	[ggsNetwork sendCommand:@"ready" withArgs:@""];
}

- (void)GGSNetwork:(GGSNetwork *)_ggsNetwork defined:(BOOL)defined {
	// do nothing.
}

- (void)GGSNetwork:(GGSNetwork *)_ggsNetwork receivedCommand:(NSString *)command withArgs:(NSString *)args {
	
	if ([command isEqualToString:@"ball"]) {
		NSArray *ball = [args componentsSeparatedByString:@","];
		[UIView beginAnimations:NULL context:NULL];
		CGFloat x = [[ball objectAtIndex:0] floatValue];
		CGFloat y = [[ball objectAtIndex:1] floatValue];
		ballView.center = CGPointMake(TOX(x), TOY(y));
		[UIView commitAnimations];
		
	} else if ([command isEqualToString:@"player1_y"]) {

		[UIView beginAnimations:NULL context:NULL];
		player1View.center = CGPointMake(25, TOY([args floatValue]));
		[UIView commitAnimations];
		
	} else if ([command isEqualToString:@"player2_y"]) {

		[UIView beginAnimations:NULL context:NULL];
		player2View.center = CGPointMake(WIDTH - 35, TOY([args floatValue]));
		[UIView commitAnimations];
		
	} else if ([command isEqualToString:@"player1_points"]) {

		pointsP1.text = args;
		gamePaused = YES;
		[lostSound play];
		
	} else if ([command isEqualToString:@"player2_points"]) {
		
		pointsP2.text = args;
		gamePaused = YES;
		[lostSound play];
		
	} else if ([command isEqualToString:@"game"]) {
		
		if ([args isEqualToString:@"wait"]) {
			
			NSLog(@"Other ready");
			
		} else if ([args isEqualToString:@"start"]) {
			
			gamePaused = NO;
			
		}
	} else if ([command isEqualToString:@"welcome"]) {
		if ([args isEqualToString:@"1"]) {
			player1View.backgroundColor = [UIColor redColor];
		} else {
			player2View.backgroundColor = [UIColor redColor];			
		}

	} else if ([command isEqualToString:@"sound"]) {
		if ([args isEqualToString:@"ping"]) {
			[pingSound play];
		} else {
			[pongSound play];
		}

	}
}

#pragma mark -
#pragma mark Input

- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
	if (gamePaused) {
		[ggsNetwork sendCommand:@"start" withArgs:@""];
		tapToBegin.hidden = YES;
	} else {
		CGPoint point = [[[touches allObjects] objectAtIndex:0] locationInView:self.view];
		if (point.y > (HEIGHT / 2)) {
			[ggsNetwork sendCommand:@"down" withArgs:@""];
		} else {
			[ggsNetwork sendCommand:@"up" withArgs:@""];
		}

	}
}


#pragma mark -
#pragma mark View

- (void)restart {
	player1View.backgroundColor = [UIColor whiteColor];
	player2View.backgroundColor = [UIColor whiteColor];
	pointsP1.text = @"0";
	pointsP2.text = @"0";
	self.ggsNetwork = [[GGSNetwork alloc] initWithDelegate:self];
	gamePaused = YES;
	tapToBegin.hidden = NO;
}

// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad {
    [super viewDidLoad];
	
	NSString *path = [[NSBundle mainBundle] pathForResource:@"ping" ofType:@"wav"];  
    pingSound = [[AVAudioPlayer alloc] initWithContentsOfURL:[NSURL fileURLWithPath:path] error:NULL];
	[pingSound play];
	
	path = [[NSBundle mainBundle] pathForResource:@"pong" ofType:@"wav"];  
    pongSound = [[AVAudioPlayer alloc] initWithContentsOfURL:[NSURL fileURLWithPath:path] error:NULL];
	[pongSound play];
	
	path = [[NSBundle mainBundle] pathForResource:@"lost" ofType:@"wav"];  
    lostSound = [[AVAudioPlayer alloc] initWithContentsOfURL:[NSURL fileURLWithPath:path] error:NULL];
	[lostSound play];

	
	//ggsNetwork = [[GGSNetwork alloc] initWithDelegate:self];

	gamePaused = YES;
	//[self startPositions];
	//[NSTimer scheduledTimerWithTimeInterval:0.05 target:self selector:@selector(moveBall) userInfo:nil repeats:YES];
}

-(void)viewDidAppear:(BOOL)animated {
    [super viewDidAppear:animated];
    [self becomeFirstResponder];
}

- (void)viewWillDisappear:(BOOL)animated {
    [self resignFirstResponder];
    [super viewWillDisappear:animated];
}


// Override to allow orientations other than the default portrait orientation.
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation {
    // Return YES for supported orientations
    return (interfaceOrientation == UIInterfaceOrientationLandscapeLeft || interfaceOrientation == UIInterfaceOrientationLandscapeRight);
}


- (void)didReceiveMemoryWarning {
	// Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
	
	// Release any cached data, images, etc that aren't in use.
}

- (void)viewDidUnload {
	// Release any retained subviews of the main view.
	// e.g. self.myOutlet = nil;
}

/*
# pragma mark -
# pragma mark Ball

- (void)moveBall {
	if (!gamePaused) {

		[UIView beginAnimations:NULL context:NULL];
		ballView.center = CGPointMake(ballView.center.x + ballVelocity.x, ballView.center.y  + ballVelocity.y );
		[UIView commitAnimations];
		
		if (ballView.center.y > HEIGHT || ballView.center.y < 0) {
			ballVelocity.y = -ballVelocity.y;
		}
		
		if (CGRectIntersectsRect(ballView.frame, player1View.frame) || CGRectIntersectsRect(ballView.frame, player2View.frame)) {
			ballVelocity.x  = - (ballVelocity.x + 1);
			if (arc4random() % 2) {
				ballVelocity.y = - (ballVelocity.y + 1);
			}
		}
		
		if (ballView.center.x > WIDTH || ballView.center.x  < 0) {
			
			if (ballView.center.x < 0) {
				pointsP1.text = [NSString stringWithFormat:@"%i", [pointsP1.text intValue] + 1];
			} else {
				pointsP2.text = [NSString stringWithFormat:@"%i", [pointsP2.text intValue] + 1];				
			}
			
			gamePaused = YES;
			[self startPositions];
		}
		
	} else {
		tapToBegin.hidden = NO;
	}
	
}

# pragma mark -
# pragma mark Positioning

- (void)startPositions {
	int s1 = - (arc4random() % 5);
	int s2 = - (arc4random() % 5);
	int d1 = arc4random() % 2 ? -1 : 1;
	int d2 = arc4random() % 2 ? -1 : 1;
	ballVelocity = CGPointMake((BALL_SPEED_X + s1) * d1 , (BALL_SPEED_Y + s2) * d2);
	ballView.center = CGPointMake(WIDTH/2, HEIGHT/2);
	player1View.center = CGPointMake(30, HEIGHT/2);
	player2View.center = CGPointMake(WIDTH-30, HEIGHT/2);
}

- (void)positionPlayer:(CGPoint)point {
	UIView *p;
	NSInteger direction = 0;
	
	if (point.x  < WIDTH/2) {
		p = player1View;
	} else {
		p = player2View;
	}
	
	if (point.y > HEIGHT/2 && p.frame.origin.y + p.frame.size.height < HEIGHT) {
		direction = 1;
	} else if (point.y < HEIGHT/2 && p.frame.origin.y > 0) {
		direction = -1;
	} else {
		direction = 0;
	}
	
	
	CGRect f = p.frame;
	f.origin.y = f.origin.y + (PLAYER_SPEED * direction);
	[UIView beginAnimations:NULL context:NULL];
	p.frame = f;
	[UIView commitAnimations];
}

#pragma mark -
#pragma mark Input

- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
	if (gamePaused) {
		tapToBegin.hidden = YES;
		gamePaused = NO;
	} else {
		switch ([touches count]) {
			case 1:
				[self positionPlayer:[[[touches allObjects] objectAtIndex:0] locationInView:self.view]];
				break;
			default:
				[self positionPlayer:[[[touches allObjects] objectAtIndex:0] locationInView:self.view]];
				[self positionPlayer:[[[touches allObjects] objectAtIndex:1] locationInView:self.view]];
				break;
		}		
	}
}


# pragma mark -
# pragma mark Reset

-(BOOL)canBecomeFirstResponder {
    return YES;
}

- (void)motionEnded:(UIEventSubtype)motion withEvent:(UIEvent *)event {
	if (event.type == UIEventSubtypeMotionShake) {
		[self zeroPoints];
	}
}

- (void)zeroPoints {
	pointsP1.text = @"0";
	pointsP2.text = @"0";
}
*/
# pragma mark -
# pragma mark Dealloc

- (void)dealloc {
	[ballView release];
	[player1View release];
	[player2View release];
	[tapToBegin release];
	[pointsP1 release];
	[pointsP2 release];
	[ggsNetwork release];
	
	[pingSound release];
	[pongSound release];
	[lostSound release];
	
    [super dealloc];
}

@end
