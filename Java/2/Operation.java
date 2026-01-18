// Peter Idestam-Almquist, 2023-02-26.

package paradis.assignment2;

class Operation implements Runnable {
	private final int ACCOUNT_ID;
	private final int AMOUNT;
	private final Account account;

	Operation(Bank bank, int accountId, int amount) {
		ACCOUNT_ID = accountId;
		AMOUNT = amount;
		account = bank.getAccount(ACCOUNT_ID);
	}

	//Used by Transaction to get access to the locks.
	Account getAccount() {
		return account;
	}

	public void run() {

		//Acquire the lock on the account, if it is already owned by the current thread, which may happen if we are running from a transaction, the hold count will be incremented instead.
		//Meaning we must release here aswell to ensure proper decrementation of the lock.
		account.blockingAcquireWriteLock(); //we are not worried about deadlocks here since we are only acquiring a single resource.
        try {
			int balance = account.getBalance();
			balance = balance + AMOUNT;
			account.setBalance(balance);
		}
		finally { //ensures the lock is always released, even in the event of an exception as to prevent poisoning.
			account.releaseWriteLock();
		}
	}
}	
