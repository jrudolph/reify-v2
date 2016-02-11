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

import scala.reflect.macros.blackbox

/**
 * An example macro that converts blocks into sequences.
 */
object CommalessSeq {
  implicit class RichAny[T](t: T) {
    def seqify: Seq[T] = macro seqifyImpl[T]
  }

  def seqifyImpl[T: ctx.WeakTypeTag](ctx: blackbox.Context): ctx.Expr[Seq[T]] =
    new Reifier {
      val c: ctx.type = ctx
      import c.universe._

      private def SafeExpr[T: WeakTypeTag](t: Tree): Expr[T] =
        Expr[T](q"$t: ${weakTypeTag[T]}")

      def run: ctx.Expr[Seq[T]] =
        c.prefix.tree match {
          case q"${ _ }.RichAny[..${ _ }](${ x @ Block(stats, res) })" ⇒
            val exprs = (stats :+ res).map(SafeExpr[T](_))
            reify {
              exprs.spliceSeq
            }
        }
    }.run
}
