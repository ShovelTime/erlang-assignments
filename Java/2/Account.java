// Peter Idestam-Almquist, 2023-02-26.

package paradis.assignment2;

import java.sql.Time;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

class Account implements Comparable<Account> {
	//Lock that determines access to the ability to modify this account.
	//If proper ordering of execution within the thread pool is preferred, then setting the fairness value to true is ideal, though with overall much worse performance of about 50%.
	private final ReentrantLock accountLock = new ReentrantLock(false);
	private final int ID;
	private volatile int balance; //Volatile to ensure proper propagation of updates.

	// Constructor.
	Account(int id, int balance) {
		ID = id;
		this.balance = balance;
	}

	// Instance methods.

	int getId() {
		return ID;
	}

	boolean tryAcquireWriteLock() {
		return accountLock.tryLock();
	}

	// alternative for when deadlocks are not a concern in the calling context.
	void blockingAcquireWriteLock() {
		accountLock.lock();
	}

	void releaseWriteLock() {
		accountLock.unlock();
	}

	int getBalance() {
		//Volatile guarantees Happens-Before relationships with the writes, and should therefore result in the most recent value being shown.
		return balance;
	}

	void setBalance(int balance) {
		if(!this.accountLock.isHeldByCurrentThread()) throw new IllegalStateException(); // no thread without a lock should be able to write here.
		this.balance = balance;
	}

	@Override
	public int compareTo(Account account) {
		// Used in sorting of the accounts for the purpose of acquiring the locks.
		return Integer.compare(this.ID, account.ID);
	}
}
