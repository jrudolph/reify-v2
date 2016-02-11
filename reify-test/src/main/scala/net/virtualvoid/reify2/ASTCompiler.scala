package net.virtualvoid.reify2

import scala.reflect.macros.blackbox

/**
 * A small example of a compiler for a simple lambda language implemented with a macro
 */
object ASTCompiler {
  object AST {
    sealed trait Expr
    // always accesses the most nested wrapping lambda parameter
    case object TheArgument extends Expr
    case class Plus(left: Expr, right: Expr) extends Expr
    case class Literal(i: Int) extends Expr

    case class Apply(lambda: Expr, argument: Expr) extends Expr
    case class Lambda(body: Expr) extends Expr
  }

  def compile(expr: AST.Expr): Int ⇒ Int = macro impl

  def impl(ctx: blackbox.Context)(expr: ctx.Expr[AST.Expr]): ctx.Expr[Int ⇒ Int] =
    new Reifier {
      val c: ctx.type = ctx

      import c.universe._
      private def parse(t: Tree): AST.Expr = t match {
        case q"${ _ }.TheArgument"                                   ⇒ AST.TheArgument
        case q"${ _ }.Plus.apply($x, $y)"                            ⇒ AST.Plus(parse(x), parse(y))
        case q"${ _ }.Literal.apply(${ Literal(Constant(i: Int)) })" ⇒ AST.Literal(i)
        case q"${ _ }.Apply.apply($lambda, $argument)"               ⇒ AST.Apply(parse(lambda), parse(argument))
        case q"${ _ }.Lambda.apply($body)"                           ⇒ AST.Lambda(parse(body))
      }

      def run: c.Expr[Int ⇒ Int] = {
        val ast = parse(expr.tree)

        /**
         * As we don't have any type-checking (yet?), we defer all potential type-checks to
         * the run time, i.e. generate `asInstanceOf` calls where necessary.
         */
        def generateAny(expr: AST.Expr, argument: Expr[Any]): Expr[Any] = expr match {
          case AST.TheArgument ⇒ argument
          case AST.Plus(x, y)  ⇒ generateInt(expr, reify(argument.splice.asInstanceOf[Int]))
          case AST.Literal(i)  ⇒ generateInt(expr, reify(argument.splice.asInstanceOf[Int]))
          case AST.Apply(lambda, arg) ⇒
            reify {
              val f = generateAny(lambda, argument).splice.asInstanceOf[Any ⇒ Any]
              val a = generateAny(arg, argument).splice
              f(a)
            }
          case AST.Lambda(body) ⇒ reify((j: Any) ⇒ generateAny(body, j.reified).splice)
        }

        def generateInt(expr: AST.Expr, argument: Expr[Int]): Expr[Int] = expr match {
          case AST.TheArgument ⇒ argument
          case AST.Plus(x, y)  ⇒ reify(generateInt(x, argument).splice + generateInt(y, argument).splice)
          case AST.Literal(i)  ⇒ c.literal(i)
          case _               ⇒ reify(generateAny(expr, argument).splice.asInstanceOf[Int])
        }
        def generateAndPrint(expr: AST.Expr, argument: Expr[Int]): Expr[Int] = {
          val res = generateInt(expr, argument)
          //println(s"Result for $expr is ${show(res.tree)}")
          res
        }

        reify {
          (i: Int) ⇒ generateAndPrint(ast, i.reified).splice
        }
      }
    }.run
}
