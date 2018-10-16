import scala.concurrent.forkjoin.ForkJoinPool

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
        if (running) return
        running = true
        thread = new Thread {
            override def run {
                while (!transactionsQueue.isEmpty) {
                    val t = transactionsQueue.pop
                    val thr = new Thread(t)
                    thr.start
                }
                running = false
            }
        }
        thread.start
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}
