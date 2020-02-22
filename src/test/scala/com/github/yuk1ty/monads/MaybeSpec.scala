package com.github.yuk1ty.monads

import com.github.yuk1ty.Member
import com.github.yuk1ty.eff.{Eff, Member}

class MaybeSpec extends funsuite.FunSuite {

  test("maybe test") {
    def e2[R[_]](implicit m: Member[Maybe, R]): Eff[R, Int] = for {
      x <- Maybe.some(2)
      y <- Maybe.none[R, Int]
    } yield x + y

    assertEqual(Eff.run(Maybe.run(e2, -1)), -1)
  }
}
