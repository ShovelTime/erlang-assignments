// Peter Idestam-Almquist, 2023-02-26.

package paradis.assignment2;

import java.util.*;
import java.util.concurrent.TimeUnit;

class Transaction implements Runnable {
	private final List<Operation> operations = new ArrayList<Operation>();
	private final Set<Account> targetedAccounts = new HashSet<>(); // Keep track of the accounts we will want to acquire a lock onto once we are ready to execute.
	private boolean closed = false;

	void add(Operation operation) {
		if (closed) return;
		operations.add(operation);
		targetedAccounts.add(operation.getAccount());
	}

	void close() {
		closed = true;
	}

	public void run() {
		if (!closed) return;
		List<Account> accounts = new ArrayList<>(targetedAccounts);
		Collections.sort(accounts);
		// If we wanted to implement rollback here, what we could do is create a hashset would contain a list of cloned accounts with their original value,
		// that could be restored if an exception occurs, this would however cause an additional performance loss due to clone semantics.

		// Wait until all the locks are acquired before running, as per the requirements of Transactions
		while(!acquireLocks(accounts)) {
			//allow more execution time to other threads for lock acquirement.
			Thread.yield();
        }
		try {
			for (Operation operation : operations) {
				operation.run();
			}
		}
		finally {
			releaseLocks(accounts);
		}
	}

	private boolean acquireLocks(List<Account> accounts) {
		List<Account> successfulLocks = new ArrayList<>(targetedAccounts.size());
		try {
			for (Account account : accounts) {
				if (account.tryAcquireWriteLock()) {
					successfulLocks.add(account);
				}
				else {
					//Free the locks so that other transaction may attempt a fully successful acquisition.
					releaseLocks(successfulLocks);
					return false;
				}

			}
		} catch (Exception e) {
			releaseLocks(successfulLocks);
		}
		return true;
	}

	private void releaseLocks(List<Account> lockedAccounts) {
		for(Account account : lockedAccounts) {
			account.releaseWriteLock();
		}
	}
}	
