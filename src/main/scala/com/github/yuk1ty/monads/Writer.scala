package com.github.yuk1ty.monads

import com.github.yuk1ty.Eff.{Impure, Pure}
import com.github.yuk1ty.EffArrows.Leaf
import com.github.yuk1ty.Union.{:+:, Inl, Inr}
import com.github.yuk1ty.Member
import com.github.yuk1ty.eff.{Eff, Member}

sealed trait Writer[+A]

case class Tell(value: String) extends Writer[Unit]

object Writer {

  def tell[R[_]](value: String)(implicit w: Member[Writer, R]): Eff[R, Unit] = Eff(Tell(value))

  def run[R[_], A](eff: Eff[(Writer :+: R)#R, A]): Eff[R, (String, A)] =
    eff match {
      case Pure(a) => Pure(("", a))
      case Impure(Inl(Tell(v)), k) => run(k(())).map { case (s, a) => (v + s, a) }
      case Impure(Inr(r), k) => Impure(r, Leaf((a: Any) => run(k(a))))
    }
}
