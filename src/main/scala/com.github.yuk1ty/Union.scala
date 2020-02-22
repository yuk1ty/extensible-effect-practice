package com.github.yuk1ty

sealed trait Union[F[_], G[_], A]

object Union {
  type :+:[F[_], G[_]] = { type R[A] = Union[F, G, A] }
  case class Inl[F[_], G[_], A](value: F[A]) extends Union[F, G, A]
  case class Inr[F[_], G[_], A](value: G[A]) extends Union[F, G, A]
}
