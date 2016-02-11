package net.virtualvoid.reify2

import scala.reflect.ClassTag
import scala.reflect.macros.blackbox

object FastArray {
  def apply[T: ClassTag](els: T*): Array[T] = macro impl[T]
  def naive[T: ClassTag](els: T*): Array[T] = macro naiveImpl[T]

  def naiveImpl[T: ctx.WeakTypeTag](ctx: blackbox.Context)(els: ctx.Expr[T]*)(tTag: ctx.Expr[ClassTag[T]]): ctx.Expr[Array[T]] =
    new Reifier {
      val c: ctx.type = ctx
      def run: c.Expr[Array[T]] =
        reify {
          implicit val ct: ClassTag[T] = tTag.splice
          val res = new Array[T](c.literal(els.size).splice)
          els.spliceSeq.copyToArray(res) // obviously this is not better than the current solution :)
          res
        }
    }.run

  def impl[T: ctx.WeakTypeTag](ctx: blackbox.Context)(els: ctx.Expr[T]*)(tTag: ctx.Expr[ClassTag[T]]): ctx.Expr[Array[T]] =
    new Reifier {
      val c: ctx.type = ctx
      def run: c.Expr[Array[T]] =
        reify {
          implicit val ct: ClassTag[T] = tTag.splice
          // create the array
          val res = new Array[T](c.literal(els.size).splice)

          // fill the array
          (0 until els.size).map { idx ⇒
            reifyInner {
              res(c.literal(idx).splice) = els(idx).splice
            }
          }.spliceStatements

          res
        }
    }.run
}
