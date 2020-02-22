package com.github.yuk1ty

sealed trait EffView[F[_], A, B]

object EffView {
  case class One[F[_], A, B](f: A => Eff[F, B]) extends EffView[F, A, B]
  case class Cons[F[_], A, B, C](f: A => Eff[F, B], k: EffArrows[F, B, C])
    extends EffView[F, A, C]
}
