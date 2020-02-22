package com.github.yuk1ty.eff

sealed trait Arrows[F[_], A, B] {

  def :+[C](f: B => Freer[F, C]): Arrows[F, A, C] = Node(this, Leaf(f))

  def ++[C](q: Arrows[F, B, C]): Arrows[F, A, C] = Node(this, q)

  def view: View[F, A, B] = this match {
    case Leaf(f) => One(f)
    case Node(l, r) =>
      @scala.annotation.tailrec
      def go[T](x: Arrows[F, A, T], y: Arrows[F, T, B]): View[F, A, B] =
        x match {
          case Leaf(f)    => Cons(f, y)
          case Node(l, r) => go(l, Node(r, y))
        }
      go(l, r)
  }

  def apply(a: A): Freer[F, B] = {
    @scala.annotation.tailrec
    def go[T](f: Arrows[F, T, B], a: T): Freer[F, B] = f.view match {
      case One(f) => f(a)
      case Cons(f, r) => f(a) match {
        case Pure(v) => go(r, v)
        case Impure(f, l) => Impure(f, l ++ r)
      }
    }
    go(this, a)
  }
}

object Arrows {
  case class Leaf[F[_], A, B](f: A => Freer[F, B]) extends Arrows[F, A, B]
  case class Node[F[_], A, B, C](left: Arrows[F, A, B], right: Arrows[F, B, C])
      extends Arrows[F, A, C]
}
