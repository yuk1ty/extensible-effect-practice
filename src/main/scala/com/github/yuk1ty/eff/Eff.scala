package com.github.yuk1ty.eff

sealed trait Eff[R[_], A] {

  def map[B](f: A => B): Eff[R, B] = flatMap(a => Pure(f(a)))

  def flatMap[B](f: A => Eff[R, B]): Eff[R, B] = this match {
    case Pure(a)      => f(a)
    case Impure(r, k) => Impure(r, k :+ f)
  }
}

object Eff {
  case class Pure[R[_], A](a: A) extends Eff[R, A]
  case class Impure[R[_], A, B](union: R[A], k: EffArrows[R, A, B])
      extends Eff[R, B]

  def apply[R[_], F[_], A](fa: F[A])(implicit F: Member[F, R]): Eff[R, A] =
    Impure(F.inject(fa), Leaf((x: A) => Pure(x)))

  def run[A](eff: Eff[Void, A]): A = eff match {
    case Pure(a) => a
  }
}
