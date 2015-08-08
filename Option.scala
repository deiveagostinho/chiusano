import scala.{Option => _, Some => _, Either => _, _}

sealed trait Option[+A]{
  def map[B](f: A => B): Option[B] =
    this match {
      case None =>  None
      case Some(a) => Some(f(a))
    }

  def getOrElse[B>:A](default: => B): B =
    this match {
      case None => default
      case Some(a) => a
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case None => None
      case Some(a) => f(a)
    }

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this match {
      case None => ob
      case Some(a) => Some(a)
    }

  def filter(f: A => Boolean): Option[A] =
    this match {
      case None => None
      case Some(a) => if (f(a)) Some(a) else None
    }
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

  def variance(xs: Seq[Double]): Option[Double] =
    this.flatMap(mean)

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(a), Some(b)) => Some(f(a, b))
    }

  def map2_2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option =
    a flatMap(aa => b map (bb => f(aa, bb)))

  def map2_3[A, B, C](a: Option[A], b: Option[b])(f: (A, B) => C): Option =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def bothMatch(pat1: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat1), mkMatcher(pat2), (f, g) => f(s) && g(s))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}
