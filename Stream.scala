import Stream._

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] =
    this match {
      case Cons(h, t) => h :: t().toListRecursive
      case _ => List()
    }

  def toList2: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] =
      s match {
        case Cons(h, t) => go(t(), h() :: acc)
        case _ => acc
      }

    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: Stream[A], i: Int): Stream[A] =
      s match {
        case Cons(h, t) if i > 0 => go(t(), cons(h(), acc), i - 1)
        case _ => acc
      }

    go(this, empty, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: Stream[A]): Stream[A] =
      s match {
        case Cons(h, t) if p(h) => go(t, cons(h, acc))
        case _ => acc
      }

    go(this, empty)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty)((a, b) => if(p(a)) cons(a, b) else empty)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty)((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty)((h, t) => if(p(h)) cons(a, b) else b)

  def append[B>:A](s: Stream[B]): Stream[B] =
    foldRight(s)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

}

object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] { def uncons = None }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def constant[A](a: A): Stream[A] =
    val as: Stream[A] = cons(a, as)
    as

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    @annotation.tailrec
    def go(s: Stream, n: Int, n1: Int): Stream[Int] =
      go(n, n + n1, cons(n + n1, s))

    go(0, 1, empty)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((h, t)) => cons(h, unfold(t)(f))
    }

  def map2[A, B](ss: Stream[A])(f: A => B): Stream[A] =
    unfold(ss)((a => Some((f(a), f(a)))))

  def onesUnfold = unfold(1)(_ => Some((1, 1)))

  def constantUnfold[A](a: A) =
    unfold(a)(_ => Some((a, a)))

  def fibsUnfold = unfold((0, 1)){ case (f1, f0) => Some(f0, (f1, f1 + f0)))}

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}
