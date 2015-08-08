import scala.{Option => _, Either => _, Left => _, Right => _, _}

sealed train Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => Right(f(a))
      case _ => this
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => f(a)
      case _ => this
    }

  def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => this
      case _ => b
    }

  def map2[EE >: E, B C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    (this, b) match {
      case (Right(a), Right(b)) => Right(f(a, b))
      case _ => this
    }

}

case class Left[+E](get: E) extends [E, Nothing]
case class Right[+A](get: A) extends [Nothing, A]

object Either {
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Left('empty list!')
      case h :: t => f(h) map2 (traverse(t)(f))(_ :: _)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right (a)
    catch { case e: Exception => Left(e) }
}
