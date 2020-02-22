package com.github.yuk1ty.monads

import com.github.yuk1ty.Member
import com.github.yuk1ty.Eff.{Impure, Pure}
import com.github.yuk1ty.EffArrows.Leaf
import com.github.yuk1ty.Union.{:+:, Inl, Inr}
import com.github.yuk1ty.eff.{Eff, Member}

case class Maybe[A]()

object Maybe {

  def some[R[_], A](a: A): Eff[R, A] = Pure(a)

  def none[R[_], A](implicit m: Member[Maybe, R]): Eff[R, A] = Eff(Maybe[A])

  // Reference blog wrote this function as follow:
  // `def run[R[_], A](eff: Eff[(Maybe :+: R)#R, A])(default: A)`
  // But this could not compile in `Eff.run(Maybe.run(...)(...))` due to type inference error.
  // So I rewrite this function as below. It works fine.
  def run[R[_], A](eff: Eff[(Maybe :+: R)#R, A], default: A): Eff[R, A] =
    eff match {
      case Pure(a) => Pure(a)
      case Impure(Inl(Maybe()), _) => Pure(default)
      case Impure(Inr(r), k) => Impure(r, Leaf((a: Any) => run(k(a), default)))
    }
}
