package books.underscoreio.advancedscala



object Functors extends App {

  /*
  * Map over functions
  * Basically f andThen f2
  * */
  import cats.syntax.functor._
  import cats.instances.function._

  val f: Int => Int = i => i + 1
  val f2: Int => Int = i => i * 2
  val f3: Int => Int = f.map(f2)
  println(f3(123))

  /*
  * Lift functions
  * */
  import cats.Functor
  import cats.instances.option._
  val liftedFforOption: Option[Int] => Option[Int] =
    Functor[Option].lift(f)
  println(liftedFforOption(Option(1)))

  import cats.instances.list._
  val liftedFforList: List[Int] => List[Int] =
    Functor[List].lift(f)
  println(liftedFforList(List(1, 2, 3)))

}
