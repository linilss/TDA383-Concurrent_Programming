import java.util.concurrent.locks.*;

class TrainMonitor {

	private final Lock lock = new ReentrantLock();
	private final Condition cond = lock.newCondition();
	private boolean isClear = true;

	public void enter() throws InterruptedException {
		lock.lock();
		try {
			while(!isClear) {
				cond.await();
			}
			isClear = true;
		}finally {
			lock.unlock();
		}
	}

	public void leave() throws InterruptedException {
		lock.lock();
		try {
			isClear = true;
			cond.signal();
		}finally {
			lock.unlock();
		}
	}

	public boolean tryEnter() throws InterruptedException {
		boolean retVal;
		lock.lock();
		try {
			if(isClear) {	
				isClear = false; 
				retVal = true;
			}
			else {
				retVal = false;
			}
		}finally {	
			lock.unlock();
		}
		return retVal;
	}
}