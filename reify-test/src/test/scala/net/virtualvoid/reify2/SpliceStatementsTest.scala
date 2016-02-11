package net.virtualvoid.reify2

import org.specs2.mutable.Specification

class SpliceStatementsTest extends Specification {
  "spliceStatements" should {
    "fast constant array example" in {
      def f(x: Int): Int = x + 42
      FastArray(1, 2, 3, 4, f(5), 6, 7, 8) must be_==(Array(1, 2, 3, 4, 47, 6, 7, 8))
    }
  }
}
