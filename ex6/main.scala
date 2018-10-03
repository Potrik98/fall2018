import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, ExecutionContext, Future}

object Hello extends App {
  val x : Array[Int] = Array(1,2,3)
  println(s"There are ${x.length} elements")
  for (i <- x) println(i)

  val numbers : Array[Int] = Array.fill(50){0}
  for (i <- 1 to 50) {
    numbers(i - 1) = i
  }

  val moreNumbers : Array[Int] = 51 to 100 toArray
  var allNumbers = numbers ++ moreNumbers

  println(sumForLoop(allNumbers))
  println(sumRecursive(allNumbers))
  println(fib(30))

  val t = createThread(() => println("test"))
  t.start()

  val printLambdas = createFibPrints(30)
  val threads = printLambdas.map(createThread)
  threads.foreach(t => t.start())

  lazy val a = 0
  lazy val future = Future {
    a
  }
  lazy val b = Await.result(future, 4.seconds)

  println(b)

  def sumForLoop(numbers: Array[Int]) : Int = {
    var sum = 0
    for (n <- numbers) {
      sum += n
    }
    sum
  }

  def createFibPrints(n : Int) : Array[() => Unit] = {
    if (n == 0) Array(() => println(fib(n)))
    else Array(() => println(fib(n))) ++ createFibPrints(n - 1)
  }

  def sumRecursive(numbers: Array[Int]) : Int = {
    if (numbers.length > 0) {
      numbers(0) + sumRecursive(numbers.slice(1, numbers.length))
    } else {
      0
    }
  }

  def fib(n: BigInt) : BigInt = {
    if (n == 0) 0
    else if (n == 1) 1
    else fib(n - 1) + fib(n - 2)
  }

  def createThread(f : () => Unit) : Thread = {
    new Thread(() => {
      f()
    })
  }
}
