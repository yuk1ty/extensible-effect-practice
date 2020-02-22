package com.github.yuk1ty

import com.github.yuk1ty.Eff.{Impure, Pure}
import com.github.yuk1ty.EffArrows.{Leaf, Node}
import com.github.yuk1ty.EffView.{Cons, One}

sealed trait EffArrows[F[_], A, B] {

  def :+[C](f: B => Eff[F, C]): EffArrows[F, A, C] = Node(this, Leaf(f))

  def ++[C](q: EffArrows[F, B, C]): EffArrows[F, A, C] = Node(this, q)

  def view: EffView[F, A, B] = this match {
    case Leaf(f) => One(f)
    case Node(l, r) =>
      @scala.annotation.tailrec
      def go[T](x: EffArrows[F, A, T], y: EffArrows[F, T, B]): EffView[F, A, B] =
        x match {
          case Leaf(f)    => Cons(f, y)
          case Node(l, r) => go(l, Node(r, y))
        }
      go(l, r)
  }

  def apply(a: A): Eff[F, B] = {
    @scala.annotation.tailrec
    def go[T](f: EffArrows[F, T, B], a: T): Eff[F, B] = f.view match {
      case One(f) => f(a)
      case Cons(f, r) => f(a) match {
        case Pure(v) => go(r, v)
        case Impure(f, l) => Impure(f, l ++ r)
      }
    }
    go(this, a)
  }
}

object EffArrows {
  case class Leaf[F[_], A, B](f: A => Eff[F, B]) extends EffArrows[F, A, B]
  case class Node[F[_], A, B, C](left: EffArrows[F, A, B], right: EffArrows[F, B, C])
    extends EffArrows[F, A, C]
}
