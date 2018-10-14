import scala.concurrent.forkjoin.ForkJoinPool
import util.control.Breaks._

class Bank(val allowedAttempts: Integer = 3) {

    private val uid = 0
    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()
    private val executorContext = 0
    private var counter: Int = 0
    private var running: Boolean = false
    private var thread: Thread = null

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
        transactionsQueue push new Transaction(
            transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
        processTransactions
    }

    // Hint: use a counter 
    def generateAccountId(): Int = this.synchronized {
        counter += 1
        return counter
    }

    private def processTransactions: Unit = {
        /*if (running) return
        running = true
        thread = new Thread {
            override def run {
                while (running) {
                    breakable {
                        if (transactionsQueue.isEmpty) {
                            Thread.sleep(50)
                            break
                        }
                        val t = transactionsQueue.pop
                        val t2 = new Thread(t)
                        t2.start()
                    }
                }
            }
        }
        thread.start*/
        while (!transactionsQueue.isEmpty) {
            val t = transactionsQueue.pop
            val t2 = new Thread(t)
            t2.start()
            t2.join(1000)
        }
    }

    def stop: Unit = {/*
        running = false
        thread.join*/
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}
