package net.virtualvoid.reify2

import scala.reflect.ClassTag
import scala.reflect.macros.blackbox

object FastArray {
  def apply[T: ClassTag](els: T*): Array[T] = macro impl[T]

  def impl[T: ctx.WeakTypeTag](ctx: blackbox.Context)(els: ctx.Expr[T]*)(tTag: ctx.Expr[ClassTag[T]]): ctx.Expr[Array[T]] = {
    trait Impl extends Reifier {
      def run: c.Expr[Array[T]] =
        reify {
          implicit val ct: ClassTag[T] = tTag.splice
          val res = new Array[T](c.literal(els.size).splice)
          els.spliceSeq.copyToArray(res) // obviously this is not better than the current solution :)
          res
        }
    }
    new Impl {
      val c: ctx.type = ctx
    }.run
  }
}
