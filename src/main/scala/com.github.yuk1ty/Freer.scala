package com.github.yuk1ty

import com.github.yuk1ty.Arrows.Leaf
import com.github.yuk1ty.Freer.{Impure, Pure}

sealed trait Freer[F[_], A] {

  def map[B](f: A => B): Freer[F, B] = flatMap(a => Pure(f(a)))

// Normal version flatMap
//  def flatMap[B](f: A => Freer[F, B]): Freer[F, B] = this match {
//    case Pure(a)       => f(a)
//    case Impure(fa, k) => Impure(fa, (a: Any) => k(a) flatMap f)
//  }

  // fast version flatMap
  def flatMap[B](f: A => Freer[F, B]): Freer[F, B] = this match {
    case Pure(a)       => f(a)
    case Impure(fa, k) => Impure(fa, k :+ f)
  }
}

object Freer {

  case class Pure[F[_], A](a: A) extends Freer[F, A]
// Normal version Impure
//  case class Impure[F[_], A, B](fa: F[A], k: A => Freer[F, B])
//      extends Freer[F, B]

  // fast version Impure
  case class Impure[F[_], A, B](fa: F[A], k: Arrows[F, A, B])
      extends Freer[F, B]

  def apply[F[_], A](ff: F[Freer[F, A]]): Freer[F, A] =
    Impure(ff, Leaf((x: Freer[F, A]) => x))
}
