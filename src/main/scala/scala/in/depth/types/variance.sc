trait UpperBound[T <: Traversable[_]] {
  def foo(a: T) = a
}
//Once refined, it's invariant
val u = new UpperBound[List[Int]] {}
u.foo(List(1,2))

trait LowerBound[T >: List[Int]] {
  def foo(a : T) = a
}
/**
 * trait only accepts types above List
 * through polymorphism, you can pass
 * any of Traversable's subtypes
 * into the foo method
 */
val l = new LowerBound[Traversable[Int]] {}
l.foo(List(1,2))
l.foo(Set(1,2)) //only Set has a different return type
l.foo(Seq(1,2))
l.foo(Iterable(1,2))
l.foo(Traversable(1,2))

/**
  *
  * The signature below won't work because
  * the constraints are strictly held on the
  * type parameter at compile time
  *
  * */
//val l2 = new LowerBound[Set[Int]] {}