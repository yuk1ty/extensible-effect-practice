package com.github.yuk1ty

sealed trait View[F[_], A, B]

object View {
  case class One[F[_], A, B](f: A => Freer[F, B]) extends View[F, A, B]
  case class Cons[F[_], A, B, C](f: A => Freer[F, B], k: Arrows[F, B, C])
      extends View[F, A, C]
}
