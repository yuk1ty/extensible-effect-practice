package com.github.yuk1ty

import com.github.yuk1ty.Union.{:+:, Inl, Inr}

trait Member[F[_], G[_]] {
  def inject[A](f: F[A]): G[A]
}

object Member {
  implicit def left[F[_], G[_]]: Member[F, (F :+: G)#R] =
    new Member[F, (F :+: G)#R] {
      override def inject[A](fa: F[A]): (F :+: G)#R[A] = Inl(fa)
    }
  implicit def right[F[_], G[_], H[_]](
      implicit member: Member[F, H]): Member[F, (G :+: H)#R] =
    new Member[F, (G :+: H)#R] {
      def inject[A](fa: F[A]): (G :+: H)#R[A] = Inr(member.inject(fa))
    }
}
