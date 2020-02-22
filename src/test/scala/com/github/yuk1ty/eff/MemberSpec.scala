package com.github.yuk1ty.eff

import com.github.yuk1ty.eff

class MemberSpec extends funsuite.FunSuite {

  test("member test") {
    val u2 = implicitly[Member[Option, (List :+: (Option :+: eff.Void)#R)#R]]
      .inject(Some(42))
    assertEqual(u2.isInstanceOf[Union[List, (Option :+: eff.Void)#R, Int]], true)
  }
}
