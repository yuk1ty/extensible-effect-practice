package com.github.yuk1ty.monads

import com.github.yuk1ty.Member
import com.github.yuk1ty.eff.Member

class IntegrationSpec extends funsuite.FunSuite {

  test("maybe write monad integration test") {
    def e3[R[_]](implicit w: Member[Writer, R], m: Member[Maybe, R]) =
      for {
        _ <- Writer.tell("hello, ")
        _ <- Maybe.none[R, Unit]
        _ <- Writer.tell("world!") // do not come here
      } yield 0

    assertEqual(Eff.run(Writer.run(Maybe.run(e3, -1))), ("hello, ", -1))
  }
}
