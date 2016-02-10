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

import scala.annotation.compileTimeOnly

import scala.reflect.macros.blackbox.Context

trait Reifier extends WithContext {
  import c.universe._
  trait Expr[+T] {
    @compileTimeOnly("splice can only be used inside of reify")
    def splice: T = ???
    def tree: Tree
  }

  trait SeqExpr[+T] {
    @compileTimeOnly("spliceSeq can only be used inside of reify")
    def spliceSeq: Seq[T] = ???
  }

  implicit def autoConv[T](exp: Context#Expr[T]): Expr[T] = new Expr[T] { def tree = exp.tree.asInstanceOf[Tree] }
  implicit def autoConvReverse[T](e: Expr[T]): c.Expr[T] = c.Expr[T](e.tree)
  implicit def convToUnit[T](exp: Expr[T]): Expr[Unit] = new Expr[Unit] { def tree = exp.tree }

  @compileTimeOnly("addSpliceSeq can only be used inside of reify")
  implicit def addSpliceSeq[T](s: Seq[Expr[T]]): SeqExpr[T] = ???
  @compileTimeOnly("addSpliceSeq can only be used inside of reify")
  implicit def addSpliceSeq2[T](s: Seq[Context#Expr[T]]): SeqExpr[T] = ???

  @compileTimeOnly("reified can only be used inside of reify")
  implicit def Reified[T](any: T): { def reified: Expr[T] } = ???

  def Expr[T](t: Tree): Expr[T] = new Expr[T] { def tree = t }
  def reify[T](t: T): Expr[T] = macro ReifierImpl.reifyImpl[T]
  def reifyShow[T](t: T): Expr[T] = macro ReifierImpl.reifyShowImpl[T]

  @compileTimeOnly("reifyInner can only be used inside of reify")
  def reifyInner[T](t: T): Expr[T] = ???
}

object ReifierImpl {
  def reifyShowImpl[T: c.WeakTypeTag](c: Context { type PrefixType = Reifier })(t: c.Expr[T]): c.Expr[c.prefix.value.Expr[T]] = {
    val res = reifyImpl(c)(t)
    c.info(t.tree.pos, s"For '${t.tree}': ${c.universe.show(res)}", false)
    res
  }
  def reifyImpl[T: c.WeakTypeTag](c: Context { type PrefixType = Reifier })(t: c.Expr[T]): c.Expr[c.prefix.value.Expr[T]] = {
    import c.universe._

    case class PlaceholderDef(orig: Tree, args: Seq[Tree], tpes: Seq[Type])
    var placeholders = Map.empty[TermName, PlaceholderDef]
    def addPlaceholder(name: TermName, ph: PlaceholderDef): Unit =
      placeholders = placeholders.updated(name, ph)

    object InnerReify {
      def unapply(tree: Tree): Option[Tree] = tree match {
        case q"${ _ }.reifyInner[..${ _ }]($exp)"      ⇒ Some(exp)
        case q"${ _ }.Reified[..${ _ }]($exp).reified" ⇒ Some(exp)
        case _                                         ⇒ None
      }
    }

    object RemoveInnerReify extends Traverser {
      var args: Seq[Tree] = _
      var tpes: Seq[Type] = _

      override def traverse(tree: Tree): Unit = tree match {
        case InnerReify(exp) ⇒
          args = args :+ CreatePlaceholders.transform(exp)
          tpes = tpes :+ exp.tpe
        case _ ⇒ super.traverse(tree)
      }

      def run(t: Tree): PlaceholderDef = {
        args = Seq.empty
        tpes = Seq.empty
        traverse(t)
        PlaceholderDef(t, args, tpes)
      }
    }

    object CreatePlaceholders extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case q"$expr.splice" ⇒
          val name = c.freshName(TermName("placeholder$"))
          val placeholder = RemoveInnerReify.run(expr)
          addPlaceholder(name, placeholder)

          q"$name(..${placeholder.args})"

        case q"${ _ }.${ TermName("addSpliceSeq") | TermName("addSpliceSeq2") }[..${ _ }]($expr).spliceSeq" ⇒
          val name = c.freshName(TermName("placeholderSeq$"))
          val placeholder = RemoveInnerReify.run(expr)
          addPlaceholder(name, placeholder)

          q"scala.collection.immutable.Seq.apply($name(..${placeholder.args}))"
        case _ ⇒ super.transform(tree)
      }
    }

    class FindDefinitions extends Traverser {
      var definitions = Set.empty[Symbol]

      override def traverse(tree: Tree): Unit = tree match {
        case v: ValDef ⇒
          definitions += v.symbol
          traverse(v.rhs)
        case _ ⇒ super.traverse(tree)
      }

      def run(t: Tree): Set[Symbol] = {
        traverse(t)
        definitions
      }
    }

    class HygienifyDefs(defs: Map[Symbol, TermName]) extends Transformer {
      override def transform(t: Tree): Tree = t match {
        case v @ ValDef(mods, name, tpt, rhs) if defs.contains(v.symbol) ⇒
          ValDef(mods, defs(v.symbol), tpt, transform(rhs))
        case x: Ident if defs.contains(x.symbol) ⇒
          //println(s"Replaced Ident($x)")
          Ident(defs(x.symbol))
        case s: Select if defs.contains(s.symbol) ⇒
          //println(s"Replaced Select($s)")
          Select(s.qualifier, defs(s.symbol))
        case _ ⇒ super.transform(t)
      }
    }

    val withPlaceholders = CreatePlaceholders.transform(t.tree)

    val allDefs = (new FindDefinitions).run(withPlaceholders)
    //println(s"Found defs: $allDefs in $t")

    val newNames = allDefs.map { s ⇒
      s -> c.freshName(TermName(s.asTerm.name.decodedName + "$"))
    }.toMap

    val freshenized = new HygienifyDefs(newNames).transform(withPlaceholders)
    val justTheNames = newNames.values.toSet

    val univ = c.typecheck(q"${c.prefix}.c.universe")

    //println(s"Before reification $freshenized")
    val reified = c.reifyTree(univ, EmptyTree, freshenized)
    //println(s"Reified: $reified")

    val pref = c.prefix
    def buildExpr[T: c.WeakTypeTag](t: Tree): Tree = q"new $pref.Expr[${c.weakTypeTag[T]}] { val tree = $t.asInstanceOf[$pref.c.universe.Tree] }"

    class InsertInnerReifies extends Transformer {
      var args = Seq.empty[Tree]
      var tpes = Seq.empty[Type]
      override def transform(tree: Tree): Tree = tree match {
        case InnerReify(_) ⇒
          val res = ReplacePlaceholder.transform(args(0))
          val tpe = tpes(0)
          args = args.tail
          tpes = tpes.tail

          buildExpr(res)(c.WeakTypeTag(tpe.widen))
        case _ ⇒ super.transform(tree)
      }

      def run(ph: PlaceholderDef, results: Seq[Tree]): Tree = {
        args = results
        tpes = ph.tpes
        transform(ph.orig)
      }
    }

    object NewTermName {
      def unapply(tree: Tree): Option[String] = tree match {
        // Scala 2.10
        case q"${ _ }.newTermName(${ Literal(Constant(name: String)) })" ⇒ Some(name)
        // Scala 2.11
        case q"${ _ }.TermName(${ Literal(Constant(name: String)) })" ⇒ Some(name)
        case _ ⇒ None
      }
    }

    object ReplacePlaceholder extends Transformer {
      def replacement(name: String, args: Seq[Tree]): Tree = {
        (new InsertInnerReifies).run(placeholders(TermName(name)), args)
      }

      override def transform(tree: Tree): Tree = tree match {
        case q"scala.collection.immutable.List.apply(${ _ }.Apply(${ _ }.Ident(${ NewTermName(name) }), ${ _ }.List.apply(..$args)))" if name.startsWith("placeholderSeq$") ⇒
          //println(s"Found Seq placeholder!!! $name\nBefore: $before\nAfter: $placed")

          val els = q"${replacement(name, args)}.map(_.tree.asInstanceOf[$$u.Tree])"
          q"scala.collection.immutable.List.apply($els: _*)"
        case q"${ _ }.Apply(${ _ }.Ident(${ NewTermName(name) }), ${ _ }.List.apply(..$args))" if name.startsWith("placeholder$") ⇒
          //println(s"Found placeholder!!! $name\nBefore: $before\nAfter: $placed")
          q"${replacement(name, args)}.tree.asInstanceOf[$$u.Tree]"

        case _ ⇒ super.transform(tree)
      }
    }

    val replaced = ReplacePlaceholder.transform(reified)
    //println(s"After placeholder replacement: $replaced")

    def createFreshName(name: TermName): Tree = q"val $name = ${c.prefix}.c.freshName(${c.prefix}.c.universe.TermName(${name.decodedName + "$"}))"
    object ReplaceFreshNames extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case NewTermName(name) if justTheNames(TermName(name)) ⇒
          //println(s"Found instance of $name: $tree")
          q"${Ident(TermName(name))}.asInstanceOf[$$u.TermName]"
        case _ ⇒ super.transform(tree)
      }
    }

    val withFreshNames =
      q"""
      ..${justTheNames.toSeq.map(createFreshName(_))}

      ${ReplaceFreshNames.transform(replaced)}
    """

    c.Expr[c.prefix.value.Expr[T]](atPos(t.tree.pos)(c.untypecheck(withFreshNames)))
  }
}
