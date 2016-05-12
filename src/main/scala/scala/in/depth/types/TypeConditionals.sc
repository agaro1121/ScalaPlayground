/**
  * IF[True,False,UpperBound]
  * */

sealed trait TBool {
  type If[TrueType <: Up, FalseType <: Up, Up] <: Up
}

class TTrue extends TBool {
  type If[TrueType <: Up, FalseType <: Up, Up] = TrueType
}
class TFalse extends TBool {
  type If[TrueType <: Up, FalseType <: Up, Up] = FalseType
}

/**
  * On True - Return String
  *
  * On False - Return Int
  * */
type X[T <: TBool] = T#If[String, Int, Any]

//val x : X[TTrue] = 5 //Fails. Takes on Type TTrue which expects String

val x : X[TTrue] = "Hi"

val y : X[TFalse] = 5