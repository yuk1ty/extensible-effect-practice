package com.github.yuk1ty.eff

class UnionSpec extends funsuite.FunSuite {

  test("union test") {
    val u1 = Inr(Inl(Some(42)))
    assertEqual(u1.isInstanceOf[(List :+: (Option :+: Void)#R)#R[Int]], true)
  }
}
