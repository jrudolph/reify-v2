/*
 *    Copyright 2016 Johannes Rudolph
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 */

package net.virtualvoid.reify2

import org.specs2.mutable.Specification

import net.virtualvoid.reify2.CommalessSeq.RichAny

class SpliceSeqTest extends Specification {
  "spliceSeq" should {
    "commaless seq example" in {
      {
        1
        2
        3
        4
        5
      }.seqify must be_==(Seq(1, 2, 3, 4, 5))
    }

    "fast constant array example" in {
      def f(x: Int): Int = x + 42
      FastConstructor.naiveArray(1, 2, 3, 4, f(5), 6, 7, 8) must be_==(Array(1, 2, 3, 4, 47, 6, 7, 8))
    }
  }
}
