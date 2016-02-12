package net.virtualvoid.reify2.future

import scala.reflect.ClassTag

/**
 * Experiments how type splicing syntax could work.
 */
object TypeExpr {
  trait Base
  trait Usage[T <: Base]

  trait TypeExpr {
    type splice
  }
  trait SubTypeExpr[B] extends TypeExpr {
    type splice <: B
  }

  val tpe: SubTypeExpr[Base] = null

  implicit val tag: ClassTag[tpe.splice] = null
  val x: Usage[tpe.splice] = null
}