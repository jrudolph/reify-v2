package net.virtualvoid.reify2

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.reflect.macros.blackbox

object FastConstructor {
  // a constructor for array's that doesn't go through the varargs Array.apply method
  // (not needed in practice, scalac makes the same optimization somewhere).
  // demonstrates `spliceStatements`
  def array[T: ClassTag](els: T*): Array[T] = macro arrayImpl[T]

  // a naive implementation of the above, which will go through Seq.apply().copyToArray
  // which wouldn't really improve anything
  // demonstrates `spliceSeq`
  def naiveArray[T: ClassTag](els: T*): Array[T] = macro naiveArrayImpl[T]

  // the same optimization for List.apply
  // demonstrate general Reifier usage
  def list[T](els: T*): List[T] = macro listImpl[T]

  def naiveArrayImpl[T: ctx.WeakTypeTag](ctx: blackbox.Context)(els: ctx.Expr[T]*)(tTag: ctx.Expr[ClassTag[T]]): ctx.Expr[Array[T]] =
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

  def arrayImpl[T: ctx.WeakTypeTag](ctx: blackbox.Context)(els: ctx.Expr[T]*)(tTag: ctx.Expr[ClassTag[T]]): ctx.Expr[Array[T]] =
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

  def listImpl[T: ctx.WeakTypeTag](ctx: blackbox.Context)(els: ctx.Expr[T]*): ctx.Expr[List[T]] =
    new Reifier {
      val c: ctx.type = ctx
      def run: c.Expr[List[T]] = {
        @tailrec def consAll(els: Seq[ctx.Expr[T]], result: Expr[List[T]]): Expr[List[T]] =
          els match {
            case head +: tail ⇒ consAll(tail, reify(head.splice :: result.splice))
            case Nil          ⇒ result
          }

        consAll(els.reverse, reify(Nil))
      }
    }.run
}
