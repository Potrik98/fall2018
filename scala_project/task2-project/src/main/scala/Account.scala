import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

    private var transactions = HashMap[String, Transaction]()

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)

    def getFullAddress: String = {
        bankId + accountId
    }

    def getTransactions: List[Transaction] = {
        // Should return a list of all Transaction-objects stored in transactions
        var list = List[Transaction]()
        for ((k,v) <- transactions) {
            list ::= v
        }
        list
    }

    def allTransactionsCompleted: Boolean = {
        // Should return whether all Transaction-objects in transactions are completed

        for ((k,v) <- transactions) {
            if (v.status == TransactionStatus.PENDING) {
                return false
            }
        }
        return true
        
    }

    def withdraw(amount: Double): Unit = {
        if (amount < 0) throw new IllegalAmountException 
        if (amount > balance.amount) throw new NoSufficientFundsException
        balance.amount -= amount
    }
    def deposit(amount: Double): Unit = {
        if (amount < 0) throw new IllegalAmountException 
        balance.amount += amount // Like in part 2
    }
    def getBalanceAmount: Double = return balance.amount // Like in part 2

    def sendTransactionToBank(t: Transaction): Unit = {
        // Should send a message containing t to the bank of this account
        println("Sending transaction to bank (" + bankId + ")")
        BankManager.findBank(bankId) ! t
    }

    def transferTo(accountNumber: String, amount: Double): Transaction = {
        val toAccount = if (accountNumber.length <= 4) bankId + accountNumber else accountNumber

        val t = new Transaction(from = getFullAddress, to = toAccount, amount = amount)

        if (reserveTransaction(t)) {
            try {
                println("current balance: " + getBalanceAmount)
                println("withdrawing " + amount)
                withdraw(amount)
                println("new balance: " + getBalanceAmount)
                sendTransactionToBank(t)

            } catch {
                case _: NoSufficientFundsException | _: IllegalAmountException => {
                    t.status = TransactionStatus.FAILED
                    println("withdrawal failed")
                }
            }
        }

        t

    }

    def reserveTransaction(t: Transaction): Boolean = {
      if (!transactions.contains(t.id)) {
        transactions += (t.id -> t)
        return true
      }
      false
    }

    override def receive = {
		case IdentifyActor => sender ! this

		case TransactionRequestReceipt(to, transactionId, transaction) => {
			// Process receipt
			transaction.receiptReceived = true
            println("recieved transaciton reciept")
            println("id: " + transactionId)
            println("status: " + transaction.status)
		}

		case BalanceRequest => getBalanceAmount // Should return current balance

		case t: Transaction => {
			// Handle incoming transaction
            println("Recieved incoming transaction:")
            println("amount: " + t.amount)
            println("from: " + t.from)
            try {
               deposit(t.amount)
               t.status = TransactionStatus.SUCCESS
            }
            catch {
                case e: Exception => (t.status = TransactionStatus.FAILED)
            }
            finally {
                sender ! TransactionRequestReceipt(t.to, t.id, t)
                System.out.println("Finished")
            }
		}

		case msg => 
    }


}
