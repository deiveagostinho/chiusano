sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r) + 1
    }
  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(v) => v
      case Branch(l, r) => size(l) max size(r)
    }
  def depth[A](t: Tree[A]): Int = ???
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  def fold[A, B](t: Tree[a])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(v) => f(v) 
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def size2[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _i)

  def maximum2(t: Tree[Int]): Int =
    fold(t)(v => v)(_ max _)

  def depth2[A](t: Tree[A]): Int =
    fold(t)(v => 0)((l, r) => 1 + (l max r))

}
