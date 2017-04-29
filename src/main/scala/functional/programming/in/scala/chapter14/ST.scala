package functional.programming.in.scala.chapter14

sealed trait ST[S, A] { self =>

  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S, A](a: => A) = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S) = (memo, s)
    }
  }
}

/**************************************************************************/
sealed trait STRef[S, A] {
  protected var cell: A

  def read: ST[S, A] = ST(cell)
  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    var cell = a
  })
}

/**************************************************************************/
sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]

  def size: ST[S, Int] = ST(value.size)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S, A] = ST(value(i))
  def freeze: ST[S, List[A]] = ST(value.toList)

  /**
   * 14.1
   */
  def fillWrongMaybe(xs: Map[Int, A]): ST[S, Unit] = {
    ST(xs.foreach {
      case (key, value) =>
        write(key, value)
    })
  }

  def fill(xs: Map[Int, A]): ST[S, Unit] =
    xs.foldRight(ST[S, Unit](())) {
      case ((k, v), st) => st flatMap (_ => write(k, v))
    }

  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] = ST(new STArray[S, A] {
    lazy val value = xs.toArray
  })

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()

  // An action that does nothing
  def noop = ST[S,Unit](())

  def partition(arr: STArray[S,Int],
                   l: Int, r: Int, pivot: Int): ST[S,Int] = for {
    pivotVal <- arr.read(pivot)
    _ <- arr.swap(pivotVal, r)
    j <- STRef(l)
    _ <- (l until r).foldLeft(noop)((s, i) => for {
      _ <- s
      vi <- arr.read(i)
      _  <- if (vi < pivotVal) (for {
        vj <- j.read
        _  <- arr.swap(i, vj)
        _  <- j.write(vj + 1)
      } yield ()) else noop
    } yield ())
    x <- j.read
    _ <- arr.swap(x, r)
  } yield x

  def qs(a: STArray[S,Int], l: Int, r: Int): ST[S,Unit] = for {
    pi <- partition(a, l, r, l + (l - r) / 2)
    _ <- qs(a, l, pi - 1)
    _ <- qs(a, pi + 1, r)
  } yield ()


}

object STArray {
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] =
    ST(new STArray[S, A] { lazy val value: Array[A] = Array.fill(sz)(v) })
}