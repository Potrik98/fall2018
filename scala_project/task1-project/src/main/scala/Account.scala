import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(amount: Double) {
        var cash: Double = amount
    }

    val balance = new Balance(initialBalance)
    val uid = bank.generateAccountId()

    def withdraw(amount: Double): Unit = this.synchronized {
        if (balance.cash < amount) {
            throw new NoSufficientFundsException
        }
        if (amount < 0) {
            throw new IllegalAmountException
        }
        balance.cash -= amount
    }

    def deposit(amount: Double): Unit = this.synchronized {
        if (amount < 0) {
            throw new IllegalAmountException
        }
        balance.cash += amount
    }

    def getBalanceAmount: Double = balance.cash

    def transferTo(account: Account, amount: Double) = this.synchronized {
        bank.addTransactionToQueue(this, account, amount)
    }
}
