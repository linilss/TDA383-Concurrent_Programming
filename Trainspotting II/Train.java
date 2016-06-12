import TSim.*;
import java.lang.*;
import java.util.*;
import java.util.concurrent.locks.*;


public class Train extends Thread {

	static TrainMonitor upperMainTrack = new TrainMonitor();
	static TrainMonitor crossing = new TrainMonitor();
	static TrainMonitor sharedUpper = new TrainMonitor();
	static TrainMonitor sharedDual = new TrainMonitor();
	static TrainMonitor sharedLower = new TrainMonitor();
	static TrainMonitor lowerMainTrack = new TrainMonitor();

	TSimInterface tsi;
	int id;
	int speed;
	boolean up;



	public Train(int id, int speed, boolean up) {
		this.id = id;
		this.speed = speed;
		this.up = up;
		tsi = TSimInterface.getInstance();

		try {
				tsi.setSpeed(id, speed);
			}catch(CommandException e) {
				e.printStackTrace();	
			}
	}

	public void boardStation() {
		up = !up;
		speed = -speed;
		try {
			tsi.setSpeed(id, 0);
			sleep(1000 + (20 * Math.abs(speed)));
			tsi.setSpeed(id, speed);
		}catch(InterruptedException e) {
			e.printStackTrace();
		}catch(CommandException e) {
			e.printStackTrace();
		}
	}

	public void makeSwitch(boolean toLeft, int switchX, int switchY) {
		try {
			if(toLeft)
				tsi.setSwitch(switchX, switchY, tsi.SWITCH_LEFT);
			else
				tsi.setSwitch(switchX, switchY, tsi.SWITCH_RIGHT);

		}catch(CommandException e) {
			e.printStackTrace();
		}

	}
	public void makeSwitch(boolean toLeft, int switchX, int switchY,
					TrainMonitor toleave, boolean leaveFromLeft) {
		try {
				if(toLeft) {
					tsi.setSwitch(switchX, switchY, tsi.SWITCH_LEFT);

					if(leaveFromLeft){
						toleave.leave();
					}
				}
				else {
					tsi.setSwitch(switchX, switchY, tsi.SWITCH_RIGHT);	
					if(!leaveFromLeft) {
						toleave.leave();
					}

				}
		}catch(CommandException e) {
			e.printStackTrace();
		}catch(InterruptedException e) {
			e.printStackTrace();
		}
	}

	public void wait(TrainMonitor s) {
		try {
			tsi.setSpeed(id, 0);
			s.enter();
			tsi.setSpeed(id, speed);

		}catch(InterruptedException e) {
		e.printStackTrace();
		}catch(CommandException e) {
			e.printStackTrace();
		}
	}
	public void waitAndSwitch(TrainMonitor s, int switchX, int switchY, 
				boolean toLeft, TrainMonitor toleave, boolean leftOrRight) {
		try {
			tsi.setSpeed(id, 0);

			s.enter();
			tsi.setSpeed(id, speed);

				if(toLeft) {
					tsi.setSwitch(switchX, switchY, tsi.SWITCH_LEFT);
					
					if(leftOrRight) {
						toleave.leave();
					}
				}
				else {
					tsi.setSwitch(switchX, switchY, tsi.SWITCH_RIGHT);	

					if(!leftOrRight) {
						toleave.leave();	
					}
				}
		}catch(InterruptedException e) {
			e.printStackTrace();
		}catch(CommandException e) {
			e.printStackTrace();
		}
	}

	public void run() {
		tsi = TSimInterface.getInstance();
		int x;
		int y;
		final boolean fromRight = false;
		final boolean fromLeft = true;

		while(true) {
			try{

				SensorEvent e = tsi.getSensor(id);
				boolean sensorActive = e.getStatus() == 1;
				x = e.getXpos();
				y = e.getYpos();

				if(sensorActive) {
					switch(x){
						case 1:
							if(up){
								makeSwitch(sharedDual.tryEnter(), 4,9);
							}
							else { 
								makeSwitch(lowerMainTrack.tryEnter(), 3,11);
							}
							break;

						case 4:
							if(up && (y == 13)){
								if(!sharedLower.tryEnter()) {
									wait(sharedLower);
								}
								tsi.setSwitch(3,11, tsi.SWITCH_RIGHT);
							}
							break;
						case 6:
							if(y == 6) {
								if(!up) {
									if(!crossing.tryEnter()) {
										wait(crossing);
									}
								}
							}
							else {
								if(up){
									if(sharedLower.tryEnter()) {
										makeSwitch((y==11), 3, 11, lowerMainTrack, fromLeft);
									}
									else {
										waitAndSwitch(sharedLower, 3, 11, 
											y==11, lowerMainTrack, fromLeft);
									}
								}
							}	 
							break;

						case 7:
							if(!up && (y >7)) {
								if(sharedLower.tryEnter()) {
									makeSwitch((y==9), 4, 9, sharedDual, fromLeft);
								}
								else{
									waitAndSwitch(sharedLower, 4, 9, y==9, sharedDual, fromLeft);
								}
							}
							break;

						case 9:
							if(!up && (y == 5)) {
								if(!crossing.tryEnter()) {
									wait(crossing);
								}
							}
							break;

						case 10:
							if(up && !crossing.tryEnter()) {
								wait(crossing);
							}
							break;
						case 11:
							if(up && !crossing.tryEnter()) {
								wait(crossing);
							}
							break;

						case 12:

							if(up) {
								if(sharedUpper.tryEnter()){
									makeSwitch((y==10), 15, 9, sharedDual, fromRight);
								}
								else {
									waitAndSwitch(sharedUpper, 15, 9, y==10, sharedDual, fromRight);
								}
							}
						break;

						case 14:
							if(y != 9) {
								if(!up) {
									if(sharedUpper.tryEnter()) {
										makeSwitch(y==8, 17, 7, upperMainTrack, fromRight);
									}
									else{
										waitAndSwitch(sharedUpper, 17,7, y==8, upperMainTrack, fromRight);
									}
								}
							}
							break;

						case 15: 
							if(up && ((y == 3) || (y == 5))) {
								boardStation();
							}
							else if(!up && ((y == 11) || (y == 13))) {
								boardStation();
							}
							break;


						case 19:
							if(up) {
								makeSwitch(!upperMainTrack.tryEnter(), 17,7);							
							}
							else {
								makeSwitch(!sharedDual.tryEnter(), 15,9);
							}
							break;
					}
				}
				else if(!sensorActive) {
					switch(x) {
						case 3:
							if(!up &&(y == 12)) {
								sharedLower.leave();
							}
						break;
						case 4: 
							if(up && (y == 10)) {
								sharedLower.leave();
							}
							else if(!up && (y == 11)) {
								sharedLower.leave();
							}
						break;
						case 5:
						 	if(up && (y == 9)) {
						 		sharedLower.leave();
						 	}
						 break;
						 case 7:
						 	if(up && (y == 7)) {
						 		crossing.leave();
						 	}
						 break;
						 case 8:
						 	if(up && (y == 6)) {
						 		crossing.leave();
						 	}
						 	else if(!up && (y == 8)) {
						 		crossing.leave();
						 	}
						 break;
						 case 9:
						 	if(!up && (y == 7)) {
						 		crossing.leave();
						 	}
						 break;
						 case 14:
						 	if(!up && (y == 9)) {
						 		sharedUpper.leave();
						 	}
						 break;
						 case 15:
 						 	if(!up && (y == 10)) {
 						 		sharedUpper.leave();
 						 	}
						 break;
						 case 16:
						 	if(up && (y == 7)) {
						 		sharedUpper.leave();
						 	}
						 break;
						 case 17:
						 	if(up && (y == 8)) {
						 		sharedUpper.leave();
						 	}
						 break;
					}
				}

			}catch(CommandException e) {
				e.printStackTrace();
			}catch(InterruptedException e) {
				e.printStackTrace();
			}

		}
	}	
}

