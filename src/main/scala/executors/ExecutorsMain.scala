package executors

import java.util.concurrent.{Callable, Executors, TimeUnit}

import scala.concurrent.{ExecutionContext, Future, Promise}

object ExecutorsMain extends App {

  val numberOfCores = Runtime.getRuntime.availableProcessors()
  private val es = Executors.newScheduledThreadPool(numberOfCores)
  implicit val ec = ExecutionContext.fromExecutor(es)

  val promise = Promise[String]()

  val value = es.schedule(new Callable[String] {
    override def call(): String = "Saluton Mondo!"
  }, 2000, TimeUnit.MILLISECONDS)

  val p = promise.success(value.get)

  p.future.onComplete(println)

  val f = Future(value.get)
  f.onComplete(println)

  es.shutdown()

}
