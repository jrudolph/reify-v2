package net.virtualvoid.reify2
package future

import ASTCompiler.AST

/**
 * Experiments to figure out how a possible syntax for type safe pattern matching could work.
 */
object Unreify {
  trait UnapplyContext { self: WithContext ⇒
    import c.universe._

    trait UnApplyer[T, U] {
      def unapply(t: Expr[T]): Option[Expr[U]]
    }
    trait UnApplyer2[T, U1, U2] {
      def unapply(t: Expr[T]): Option[(Expr[U1], Expr[U2])]
    }

    // e.g.
    // extract from the AST example
    // case q"${_}.Plus(x, y)" =>

    // or
    // case q"${ _ }.Literal.apply(${ Literal(Constant(i: Int)) })" =>

    def unreify[T, U](pf: Nothing ⇒ T): UnApplyer[T, U] = ???
    def unreify[T, U1, U2](pf: (Nothing, Nothing) ⇒ T): UnApplyer2[T, U1, U2] = ???

    val Literal: UnApplyer[AST.Expr, Int] =
      unreify(AST.Literal(_))

    val Plus: UnApplyer2[AST.Expr, AST.Expr, AST.Expr] =
      unreify(AST.Plus(_, _))

    val ast: Expr[AST.Expr] = ???

    ast match {
      case Literal(i)              ⇒ //
      case Plus(Literal(i), right) ⇒ //
    }
  }
}
