/**
  * Created by Hierro on 5/7/16.
  */
package object complexmath {
  implicit def realToComplex(r : Double) = new ComplexNumber(r, 0.0)
  val i = ComplexNumber(0.0, 1.0)
}
