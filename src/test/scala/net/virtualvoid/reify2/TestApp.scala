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

import scala.reflect.macros.blackbox.Context

object TestApp {
  def test(i: Int): String = macro testMacro

  def show[T](t: T): T = macro showTree[T]
  def showTree[T](c: Context)(t: c.Expr[T]): c.Expr[T] = { println(s"Show '${c.universe.show(t)}'"); t }

  abstract class Impl[C <: Context](ctx: C) extends Reifier {
    val c: C = ctx
  }

  def testMacro(c: Context)(i: c.Expr[Int]): c.Expr[String] = new Impl[c.type](c) {
    def transform(i: Expr[Int], j: Int): Expr[Int] = reifyShow {
      val x = i.splice

      (if (j > 0) (transform(x.reified, j - 1).splice + 23).reified else (x + 42).reified).splice
    }
    def res: c.Expr[String] = {
      val res = reify {
        var x = 42

        transform(x.reified, 10).splice.toString
        //(x + (38 * i.splice).reified.splice).reified.splice.toString
        //transform(x.reified).splice(38).toString
      }
      println(c.universe.show(res.tree))
      res
    }
  }.res
}
