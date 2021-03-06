import java.util.NoSuchElementException

import akka.actor._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import akka.util.Timeout

case class GetAccountRequest(accountId: String)

case class CreateAccountRequest(initialBalance: Double)

case class IdentifyActor()

class Bank(val bankId: String) extends Actor {

    val accountCounter = new AtomicInteger(1001)

    def createAccount(initialBalance: Double): ActorRef = {
        // Should create a new Account Actor and return its actor reference. Accounts should be assigned with unique ids (increment with 1).
        BankManager.createAccount(accountCounter.getAndIncrement.toString, bankId, initialBalance)
    }

    def findAccount(accountId: String): Option[ActorRef] = {
        // Use BankManager to look up an account with ID accountId
        Some(BankManager.findAccount(bankId, accountId))
    }

    def findOtherBank(bankId: String): Option[ActorRef] = {
        // Use BankManager to look up a different bank with ID bankId

        Some(BankManager.findBank(bankId))
    }

    override def receive = {
        case CreateAccountRequest(initialBalance) => sender ! createAccount(initialBalance) // Create a new account
        case GetAccountRequest(id) => sender ! findAccount(id) // Return account
        case IdentifyActor => sender ! this
        case t: Transaction => processTransaction(t)

        case t: TransactionRequestReceipt => {
        // Forward receipt
            val toBankId = t.toAccountNumber.substring(0, 4)
            if (toBankId == bankId) {
                val someAccount = findAccount(t.toAccountNumber)
                if (!someAccount.isEmpty) {
                    someAccount.get ! t
                }
            }
            else {
                val someBank = findOtherBank(t.toAccountNumber.substring(0, 4))
                if (!someBank.isEmpty) {
                    someBank.get ! t
                }
            }
//            sender ! TransactionRequestReceipt(t.to, t.id, t)
        }

        case msg => ???
    }

    def processTransaction(t: Transaction): Unit = {
        implicit val timeout = new Timeout(5 seconds)
        println("bank " + bankId + " processing transaction")
        println("to: " + t.to)
        val toBankId = t.to.substring(0, 4)
        val isInternal = toBankId == bankId
        println("internal: " + isInternal)
        println("toBankId: " + toBankId)
        println("transactionStatus: " + t.status)
        
        // This method should forward Transaction t to an account or another bank, depending on the "to"-address.
        // HINT: Make use of the variables that have been defined above.
        if (isInternal) {
            findAccount(t.to).get ! t
        }
        else {
            findOtherBank(toBankId).get ! t
        }
    }
}