package com.github.yuk1ty.eff

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
