import exceptions._
import scala.collection.mutable
import scala.collection.mutable.Queue

object TransactionStatus extends Enumeration {
    val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {
    val queue = new Queue[Transaction]()

    // Remove and return the first element from the queue
    def pop: Transaction = queue.dequeue

    // Return whether the queue is empty
    def isEmpty: Boolean = queue.isEmpty

    // Add new element to the back of the queue
    def push(t: Transaction): Unit = queue.enqueue(t)

    // Return the first element from the queue without removing it
    def peek: Transaction = queue.front

    // Return an iterator to allow you to iterate over the queue
    def iterator: Iterator[Transaction] = queue.iterator

    def size: Int = queue.size
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

    var status: TransactionStatus.Value = TransactionStatus.PENDING

    override def run: Unit = {
        var attempts = 0

        def doTransaction() = {
            try {
                println("Attempting transaction of " + amount)
                from withdraw amount
                to deposit amount
                status = TransactionStatus.SUCCESS
                processedTransactions.push(this)
            } catch {
                case e: Exception => attempts += 1
            }
        }

        while (status == TransactionStatus.PENDING && attempts < allowedAttemps) {
            if (from.uid < to.uid) from synchronized {
                to synchronized {
                    doTransaction
                }
            } else to synchronized {
                from synchronized {
                    doTransaction
                }
            }
        }

        if (attempts == allowedAttemps) {
            status = TransactionStatus.FAILED
            processedTransactions.push(this)
        }

        println("Transaction competed with status " + status +
                ". Processed: " + processedTransactions.size +
                ", left: " + transactionsQueue.size)
    }
}
