// Peter Idestam-Almquist, 2023-02-26.

package paradis.assignment2;

import java.util.List;
import java.util.ArrayList;
import java.util.concurrent.CopyOnWriteArrayList;

//Not much to be done here, as this is inherently a wrapper for a list.

class Bank {
	// Instance variables.
	private final List<Account> accounts = new CopyOnWriteArrayList<>(); // Allow multiple thread to add and access accounts.

	// Instance methods.

	int newAccount(int balance) {
		int accountId = accounts.size();
		accounts.add(new Account(accountId, balance));
		return accountId;
	}

	Account getAccount(int accountId) {
		Account account = accounts.get(accountId);
		return account;
	}
}
