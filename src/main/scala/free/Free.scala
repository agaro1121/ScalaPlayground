package free

import free.FunctorTransformer.{Id, ~>}
import scala.language.higherKinds

/**************************** Boilerplate ********************************/
trait Functor[F[_]] {
  def map[A, B](a: F[A])(fn: A => B): F[B]
}

trait Monad[M[_]] extends Functor[M] {
  def pure[A](a: A): M[A]

  def flatMap[A, B](a: M[A])(fn: A => M[B]): M[B]

  def map[A, B](a: M[A])(fn: A => B): M[B] =
    flatMap(a)(b => pure(fn(b)))

  /* Usually comes from Monoid. Not required here

  def append[A, B, C](f1: A => M[B], f2: B => M[C]): A => M[C] = { a: A =>
    val bs: M[B] = f1(a)
    val cs: M[C] = flatMap(bs) { b: B => f2(b) }
    cs
  }

  def identity[A]: A => M[A] = a => pure(a)
  */
}

object Monad {
  def apply[F[_]: Monad]: Monad[F] = implicitly[Monad[F]]

  // make id a monad so we can use it in our `runFree` code
  implicit val idMonad = new Monad[Id] {
    def pure[A](given: A): Id[A] = given
    def flatMap[A, B](given: Id[A])(fn: A => Id[B]): Id[B] = fn(given)
  }
}


/************************ The Free Stuff ***********************************
* Store your functions as data as you go so you don't lose anything
* And you don't evaluate until the very end
*
* program returning `A` with instructions `F[_]` or family of types `F[_]`
* */
trait Free[F[_], A] {
  import Free._

  def flatMap[B](f: A ⇒ Free[F, B]): Free[F, B] = FlatMap(this, f)
  def map[B](f: A ⇒ B): Free[F, B] = flatMap(a ⇒ pure(f(a)))
}

/*
* or `Return` as in Haskell
* No more computation to be done
* */
case class Pure[F[_], A](value: A) extends Free[F, A]

/*
* Suspend computation until we're ready to process data
* */
case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

case class FlatMap[F[_], A, B](free: Free[F, A], f: A ⇒ Free[F, B]) extends Free[F, B]

object Free {
  //smart constructors
  def pure[F[_], A](value: A): Free[F, A] = Pure(value)
  def suspend[F[_], A](s: F[A]): Free[F, A] = Suspend(s)
}

/*************************************************************************/
trait FunctorTransformer[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}
object FunctorTransformer {
  type ~>[F[_], G[_]] = FunctorTransformer[F, G]
  type Id[A] = A
}

object GenericInterpreter {
  def runFree[F[_], G[_]: Monad, A](f: Free[F, A])(transform: FunctorTransformer[F, G]): G[A] = {

    //Trampoline this bitch
    @annotation.tailrec
    def tailThis(free: Free[F, A]): Free[F, A] =
      free match {
        case FlatMap(FlatMap(fr, fn1), fn2) => tailThis(fr.flatMap(a1 => fn1(a1).flatMap(a2 => fn2(a2))))
        case FlatMap(Pure(a), fn) => tailThis(fn(a))
        case _ => free
      }

    val G = Monad[G] // uses implicit objects in constructor

    tailThis(f) match {
      case Pure(a) => implicitly[Monad[G]].pure(a) //can also pull the implicit like this
      case Suspend(fa) => transform(fa)
      case FlatMap(Suspend(fa), fn) => G.flatMap(transform(fa)) { a => runFree(fn(a))(transform) }
      case _ => throw new AssertionError("Unreachable")
    }
  }

}
/********************* EXAMPLE Using the Free Stuff *************************/
sealed trait Todo[A]
case class NewTask[A](task: A) extends Todo[A]
case class Complete[A](task: A) extends Todo[A]
case class GetTasks[A](default: A) extends Todo[A]

object Todo {
  def newTask[A](task: A): Free[Todo, A] =
    Free.suspend(NewTask(task))

  def completeTask[A](task: A): Free[Todo, A] =
    Free.suspend(Complete(task))

  def getTasks[A](default: A): Free[Todo, A] =
    Free.suspend(GetTasks(default))
}

object TodoExampleMonad {
  import Todo._
  val todos: Free[Todo, Map[String, Boolean]] =
    for {
      _ ← newTask("Learn about Functor")
      _ ← newTask("Learn about Monads")
      _ ← newTask("Buy a Unicorn")
      _ ← completeTask("Learn about Monads")
      tsks ← getTasks(Map.empty[String, Boolean])
    } yield tsks
}
/*************************** Run This Ish *******************************/
case object PrintEvaluator extends (Todo ~> Id) /*FunctorTransformer[Todo, Id]*/ {
  override def apply[A](a: Todo[A]): A = {
    a match {
      case NewTask(task) =>
        println(s"New task added: $task")
        task
      case Complete(task) =>
        println(s"Task completed: $task")
        task
      case GetTasks(default) =>
        println(s"Request to fetch tasks")
        default
    }
  }
}

case object ProductionEvaluator extends (Todo ~> Id) /*FunctorTransformer[Todo, Id]*/ {
  var results: Map[String, Boolean] = Map.empty

  def apply[A](a: Todo[A]): A = {
    a match {
      case NewTask(task) =>
        results = results + (task.toString → false)
        task
      case Complete(task) =>
        results = results + (task.toString → true)
        task
      case GetTasks(_) =>
        results.asInstanceOf[A]
    }
  }
}

/*************************** Main Method ********************************/
object Tester extends App {
  private val result: Id[Map[String, Boolean]] = GenericInterpreter.runFree[Todo, Id, Map[String, Boolean]](TodoExampleMonad.todos)(ProductionEvaluator)
  println("In the end...\n\n" + result.mkString("\n"))
}