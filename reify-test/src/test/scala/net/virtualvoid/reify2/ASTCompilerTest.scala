package net.virtualvoid.reify2

import org.specs2.mutable.Specification

import net.virtualvoid.reify2.ASTCompiler.AST

class ASTCompilerTest extends Specification {
  "ASTCompiler" should {
    "compile constant functions" in {
      val f = ASTCompiler.compile(AST.Plus(AST.Literal(12), AST.Literal(42)))

      f(38) must be_==(54)
      f(12) must be_==(54)
    }
    "compile funcs that use argument" in {
      val f = ASTCompiler.compile(AST.Plus(AST.Literal(23), AST.TheArgument))

      f(19) must be_==(42)
      f(0) must be_==(23)
    }
    "compile lambda's and applications" in {
      val f = ASTCompiler.compile {
        // Represents the function
        // {
        //   val double = x => x + x
        //   y => double(y + 23)
        // }
        AST.Apply(
          AST.Lambda(AST.Plus(AST.TheArgument, AST.TheArgument)), // doubles the argument
          AST.Plus(AST.TheArgument, AST.Literal(23)))
      }

      f(19) must be_==(84)
      f(0) must be_==(46)
    }
    "pass lambda to lambda" in {
      val f = ASTCompiler.compile {
        // {
        //   val app = (f: Int => Int) => f(38)
        //   app(x => x + x)
        // }
        AST.Apply(
          AST.Lambda(AST.Apply(AST.TheArgument, AST.Literal(38))),
          AST.Lambda(AST.Plus(AST.TheArgument, AST.TheArgument)))
      }

      f(0) must be_==(76)
      f(1) must be_==(76)
    }
  }
}
