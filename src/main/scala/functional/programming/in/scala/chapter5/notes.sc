false && { println("!!"); true} //did not print
true && { println("!!"); true}

true || { println("!!"); true} //did not print
false || { println("!!"); true}

true | { println("!!"); true} //forces it to evaluate
false & { println("!!"); true} //strict evaluation


//////////////////////////////////////////////////////
/*
  Scala evaluates i every time it is invoked
  It is invoked here twice
*/
def maybeTwice(b: Boolean, i: ⇒Int) =
  if(b) i+i else 0
val x = maybeTwice(true, { println("hi"); 1 + 41})

/*
  Scala evaluates j once once since since `Lazy` caches the value
*/
def maybeTwiceLazy(b: Boolean, i: ⇒Int) = {
  lazy val j = i
  if(b) j+j else 0
}
val y = maybeTwiceLazy(true, { println("hi"); 1 + 41})

