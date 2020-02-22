package com.github.yuk1ty.monads

import com.github.yuk1ty.Member
import com.github.yuk1ty.eff.Member

class WriterSpec extends funsuite.FunSuite {

  test("writer test") {
    def e1[R[_]](implicit w: Member[Writer, R]) = for {
      _ <- Writer.tell("hello, ")
      _ <- Writer.tell("world.")
    } yield 0

    assertEqual(Eff.run(Writer.run(e1)), ("hello, world.", 0))
  }
}
