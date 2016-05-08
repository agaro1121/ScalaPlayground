package scala.in.depth.types

/**
  * Created by Hierro on 5/7/16.
  */
trait Observable {
  type Handle
  var callbacks = Map[Handle, this.type => Unit]()

  def observe(callback: this.type => Unit): Handle = {
    val handle = createHandle(callback)
    callbacks += (handle -> callback)
    handle
  }

  def unobserve(handle: Handle): Unit = {
    callbacks -= handle
  }

  protected def createHandle(callback: this.type => Unit): Handle

  protected def notifyListeners(): Unit =
    for (callback <- callbacks.values) callback(this)
}

trait DefaultHandles extends Observable {
  type Handle = (this.type => Unit)

  protected def createHandle(callback: this.type => Unit): Handle =
    callback
}

class IntStore(private var value: Int)
  extends Observable with DefaultHandles {
  def get : Int = value
  def set(newValue : Int) : Unit = {
    value = newValue
    notifyListeners()
  }
  override def toString : String = "IntStore(" + value + ")"
}

object ObservableTest extends App {
  val x = new IntStore(5)
  val handle = x.observe(println)
  x.set(2)
  x.unobserve(handle)
  x.set(4)

}