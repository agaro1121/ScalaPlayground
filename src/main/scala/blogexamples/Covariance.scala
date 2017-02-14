package blogexamples

class Cell                      { override def toString: String = "Cell"     }
class Animal   extends Cell     { override def toString: String = "Animal"   }
trait Primeape extends Animal   { override def toString: String = "Primeape" }
trait Caveman  extends Primeape { override def toString: String = "Caveman"  }
class Human    extends Caveman  { override def toString: String = "Human"    }

// CoVariance
trait AcceptsAnythingBelowCaveman[+Caveman]

// ContraVariance
trait AcceptsAnythingAbovePrimeape[-Primeape]



object Tester extends App {

  new AcceptsAnythingAbovePrimeape[Animal]{}
  new AcceptsAnythingBelowCaveman[Human]{}

  new AcceptsAnythingAbovePrimeape[Human]{} //fails - throws exception
  new AcceptsAnythingBelowCaveman[Animal]{} //fails - throws exception

}

object WithMethods extends App {

  val animal = new Animal()
  val human = new Human()
  val cell = new Cell

  def acceptsAnythingAbovePrimeape[T >: Primeape](t: T): Unit = println(t)

  def acceptsAnythingBelowCaveman[T <: Caveman](t: T): Unit = println(t)

  def acceptsOnlyCaveman(t: Caveman): Unit = println(t)

  acceptsAnythingAbovePrimeape(animal) // works as expected

  /*
  * Human is a Caveman
  * Caveman is a Primeape
  * ergo, Human is a Primeape
  *
  * In this scenario, Scala will cast human up to a Primeape to make it work
  * To be clear, this method accepts anything Primeape and above, including Primeape
  * */
  acceptsAnythingAbovePrimeape(human)

  acceptsAnythingBelowCaveman(human) //works as expected

//  acceptsAnythingBelowCaveman(animal) //fails - throws exception

//  acceptsOnlyCaveman(animal)
  acceptsOnlyCaveman(human) //works - human is a caveman
//  acceptsOnlyCaveman(cell) //fails - cell is not a caveman
}

