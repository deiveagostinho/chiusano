import scala.{Option => _, Some => _, Either => _, _}

sealed trait Option[+A]{
  def map[B](f: A => B): Option[B] = ???

  def getOrElse[B>:A](default: => B): B = ???

  def flatMap[B](f: A => Option[B]): Option[B] = ???

  def orElse[B>:A](ob: => Option[B]): Option[B] = ???

  def filter(f: A => Boolean): Option[A] = ???
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Expection => 43 }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int)
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = ???
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???
  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}
