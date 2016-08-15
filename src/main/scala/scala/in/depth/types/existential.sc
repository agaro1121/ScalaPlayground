val y : List[_] = List()
//same shit except now we have a handle on the existential type
val x: List[X forSome {type X}] = y

//with Bounds
val a: List[_ <: AnyRef] = List()
val b: List[X forSome {type X <: AnyRef}] = a