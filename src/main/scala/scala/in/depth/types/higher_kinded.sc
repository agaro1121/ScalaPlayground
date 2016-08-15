type Callback[T] = Function1[T,Unit]

def x : Callback[Int] = y => println(y + 2)

def foo[M[_]](f: M[Int]) = f

foo[Callback](x)
//Using type Lambdas, you can construct Callback[T] as shown below
foo[({type C[T] = Function1[T,Unit]})#C](x)

/*
  Observable
 */

trait Observable {
  var callbacks = scala.collection.mutable.Map[Handle,this.type => Unit]()
  type Handle
  def observe(callback: this.type => Unit): Handle = {
    val handle = createHandle(callback)
    callbacks += (handle -> callback)
    handle
  }
  def unobserve(handle: Handle) : Unit = {
    callbacks -= handle
  }
  protected def createHandle(callback: this.type => Unit): Handle
  protected def notifyListeners() : Unit =
    for(callback <- callbacks.values) callback(this)
}

trait DefaultObservable extends Observable {
  override type Handle = (this.type => Unit)

  override protected def createHandle(callback: (this.type) => Unit): Handle = callback
}

class IntStore(private var value: Int) extends Observable
  with DefaultObservable {
  def getValue: Int = value
  def setValue(v: Int) = {
    value = v
    notifyListeners()
  }
  override def toString : String = "IntStore(" + value + ")"
}

val intStore = new IntStore(5)
val observer1 = intStore.observe(println)
val observer2 = intStore.observe(println)
intStore.setValue(2)
intStore.setValue(4)

