package scala.in.depth.types

/**
  * Created by Hierro on 5/7/16.
  */
class Outer {
  trait Inner
  def y = new Inner {}
  def foo(x : this.Inner) = null //only takes path-dependent - inner must be specific instance of Outer passed in
  def bar(x : Outer#Inner) = null //inner can be from any instance of Outer
}


/**
  * Below shows path-dependent vs Project Types

    scala> val x = new Outer
    x: Outer = Outer@4c3cf920

    scala> val y = new Outer
    y: Outer = Outer@d6c643

    scala> x.y
    res2: x.Inner = Outer$$anon$1@72e48ab3

    scala> x.foo(x.y)
    res3: Null = null

    scala> x.foo(y.y)
    <console>:19: error: type mismatch;
     found   : y.Inner
     required: x.Inner
           x.foo(y.y)
                   ^

    scala> x.bar(y.y)
    res5: Null = null

    scala> x.bar(x.y)
    res6: Null = null

  * */