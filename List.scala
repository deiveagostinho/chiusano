sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def head[A](l: List[A]): A =
    l match {
      case Cons(h, t) => h
    }

  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => t
    }

  def drop[A](l: List[A], n: Int): List[A] =
    n match {
      case 0 => l
      case _  => drop(tail(l), n - 1)
    }

  def dropWhile[A](l: List[A])(p: A => Boolean): List[A] =
    if (p(head(l))) dropWhile(tail(l))(p)
    else l

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def setHead[A](l: List[A])(a: A): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(a, t)
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(h, Nil) => Nil
      case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
      case Cons(h, t) => Cons(h, init(t))
    }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_+_)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_*_)

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    def go(xs: List[A], acc: B): B =
      xs match {
        case Nil => acc
        case Cons(h, t) => go(t, f(acc, h))
      }

    go(l, z)
  }

  def sum3(l: List[Int]) =
    foldLeft(l, 0)(_+_)

  def product3(l: List[Double]) =
    foldLeft(l, 0)(_*_)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((x, _) => x + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil)((x, y) => Cons(y, x))

  def append[A](l: List[A], k: List[A]): List[A] =
    foldRight(k, l)(Cons(_,_))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil)(append)

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = ???
  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = ???

  def map[A, B](l: list[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h, t) => Cons(f(h), t))

  def filter[A](l: List[A])(p: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((h, t) => if (p(h)) Cons(h, t) else t)

  def inc(l: List[Int]): List[Int] =
    map(l)((x) => x + 1)

  def doubleToString(l: List[Double]): List[String] =
    map(l)((x) => x.toString())

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filter2[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)((x) => if (p(x)) Cons(x, Nil) else Nil)

  def addCorrespondence(l: List[Int], k: List[Int]): List[Int] =
    (l, k) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addCorrespondence(xs, ys))
    }

  def zipWith[A, B, C](l: List[A], k: List[B])(f: (A, B) => C): List[C] =
    (l, k) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys, f))
    }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = ???
}
