package reactive
package web
package js

import scala.reflect.macros.Context

import reactive.web.js.{ JsAst => j }

object Macros {
  trait MacroUtils {
    val context: Context
    lazy val c: context.type = context
    import context.universe._

    implicit class treeOps(tree: Tree) {
      def sel(name: TermName) = Select(tree, name)
      def ap(args: List[Tree]) = Apply(tree, args)
      def tAp(tArgs: List[Tree]) = TypeApply(tree, tArgs)
    }
    implicit class stringOps(s: String) {
      def sel(name: TermName) = Ident(newTermName(s)) sel name
    }

    def list(els: List[Tree]) = reify{ List }.tree ap els

    def trace(in: Tree)(out: Tree) = {
      println("in: "+in)
      println("out: "+out)
      out
    }

    object Mods {
      val flags = List(
        Flag.TRAIT, Flag.INTERFACE, Flag.MUTABLE, Flag.MACRO, Flag.DEFERRED,
        Flag.ABSTRACT, Flag.FINAL, Flag.SEALED, Flag.IMPLICIT, Flag.LAZY, Flag.OVERRIDE,
        Flag.PRIVATE, Flag.PROTECTED, Flag.LOCAL, Flag.CASE, Flag.ABSOVERRIDE, Flag.BYNAMEPARAM,
        Flag.PARAM, Flag.COVARIANT, Flag.CONTRAVARIANT, Flag.DEFAULTPARAM, Flag.PRESUPER, Flag.DEFAULTINIT
      )
      def unapplySeq(m: Modifiers) = Some(flags.filter(m.hasFlag))
    }
  }

  class JsCompiler[C <: Context](val context: C) extends MacroUtils {
    import context.universe._
    def apply(body: context.Expr[Any]): context.Expr[j.Statement] = {
      def jsAst = reify{ j }.tree

      def block(trees: Tree*) = reify { j.Block }.tree ap List(list(trees.toList))
      def str(s: String) = reify{ j.LitStr(c.literal(s).splice) }.tree
      def bool(b: Boolean) = reify{ j.LitBool(c.literal(b).splice) }.tree
      def num(n: Tree) = {
        val e = c.Expr(n)
        reify{ j.LitNum(e.splice) }.tree
      }
      def declare(n: String) = reify{ j.Declare(c.literal(n).splice) }.tree
      def assign(to: Tree, what: Tree) = {
        val t = c.Expr(to)
        val w = c.Expr(what)
        reify{ j.Assign(t.splice, w.splice) }.tree
      }
      def simpleIdent(n: String) = reify{ j.SimpleIdentifier(c.literal(n).splice) }.tree
      def select(q: Tree, n: String) = {
        val qe = c.Expr(q)
        reify{ j.Select(qe.splice, c.literal(n).splice) }.tree
      }
      def _apply(fun: Tree, args: List[Tree]) = reify{ j.Apply }.tree ap fun +: args
      def _if(cond: Tree, yes: Tree, no: Tree) = {
        val ce = c.Expr(cond)
        val y = c.Expr(yes)
        val n = c.Expr(no)
        reify{ j.If(ce.splice, y.splice, n.splice) }.tree
      }
      def _while(cond: Tree, body: Tree) = {
        val ce = c.Expr(cond)
        val b = c.Expr(body)
        reify{ j.While(ce.splice, b.splice) }.tree
      }
      def doWhile(body: Tree, cond: Tree) = {
        val b = c.Expr(body)
        val ce = c.Expr(cond)
        reify{ j.DoWhile(b.splice, ce.splice) }.tree
      }
      def switch(in: Tree, cases: List[Tree], default: Tree) = {
        val i = c.Expr(in)
        val cs = c.Expr(list(cases))
        val d = c.Expr(default)
        reify{ j.Switch(i.splice, cs.splice, d.splice) }.tree
      }
      def _case(test: List[Tree], st: List[Tree]) = {
        val t = c.Expr(list(test))
        val s = c.Expr(list(st))
        reify{ j.Case(t.splice, s.splice) }.tree
      }
      def _throw(e: Tree) = {
        val ee = c.Expr(e)
        reify{ j.Throw(ee.splice) }.tree
      }
      def _try(b: List[Tree], n: String, cs: List[Tree], f: List[Tree]) = {
        val be = c.Expr(list(b))
        val ce = c.Expr(list(cs))
        val fe = c.Expr(list(f))
        reify{ j.Try(be.splice, c.literal(n).splice, ce.splice, fe.splice) }.tree
      }

      @inline def exprPF(pf: PartialFunction[Tree, Tree]) = pf

      val exprLitStr = exprPF {
        case Literal(Constant(s: String)) => str(s)
      }
      val exprLitBool = exprPF {
        case Literal(Constant(b: Boolean)) => bool(b)
      }
      val exprLitNum = exprPF {
        case n @ Literal(Constant((_: Int) | (_: Double) | (_: Long) | (_: BigDecimal))) =>
          num(n)
      }
      lazy val exprSelect = exprPF {
        case Select(left, right) =>
          if(!(left.tpe <:< typeOf[js.Object]))
            c.error(left.pos, "Can only select members of a js.Object")
          select(expr(left), right.toString)
      }
      lazy val exprIdent = exprPF {
        case Ident(name) => simpleIdent(name.toString)
      }

      @inline def stPF(pf: PartialFunction[Tree, List[Tree]]) = pf

      def expr(t: Tree): Tree =
        exprLitStr orElse
          exprLitBool orElse
          exprLitNum orElse
          exprSelect orElse
          exprIdent applyOrElse(t, {
              c.info(t.pos, s"Error converting code to javascript, input is: $body\n  raw: ${showRaw(body)}", false)
              c.abort(t.pos, s"Don't know how to convert expression $t\n raw: ${showRaw(t)}")
          }) setPos t.pos

      val stValVar = stPF {
        case ValDef(m, name, TypeTree(), e) if m == Modifiers() || m == Modifiers(Flag.MUTABLE) =>
          List(
            declare(name.toString),
            assign(simpleIdent(name.toString), expr(e))
          )
      }
      val stAssign = stPF {
        case Assign(name, e) =>
          List(assign(simpleIdent(name.toString), expr(e)))
      }
      lazy val stIf = stPF {
        case If(cond, yes, no) =>
          List(
            _if(expr(cond), block(statements(yes): _*), block(statements(no): _*))
          )
      }
      lazy val stWhile = stPF {
        case LabelDef(
          labelName,
          Nil,
          If(
            cond,
            Block(b, Apply(Ident(jumpName), Nil)),
            Literal(Constant(()))
          )
        ) if labelName == jumpName =>
          List(
            _while(expr(cond), block(b flatMap (s => statements(s)): _*))
          )
      }
      lazy val stDoWhile = stPF {
        case LabelDef(
          labelName,
          Nil,
          Block(
            b,
            If(
              cond,
              Apply(Ident(jumpName), Nil),
              Literal(Constant(()))
            )
          )
        ) if labelName == jumpName =>
          List(
            doWhile(block(b flatMap (s => statements(s)): _*), expr(cond))
          )
      }
      lazy val stSwitch = stPF {
        case t @ Match(input, casedefs) =>
          def openBlock(t: Tree) = t match {
            case Block(stmts, ret) => stmts :+ ret flatMap (s => statements(s))
            case _                 => statements(t)
          }
          val allCases: Map[Boolean, List[List[Tree]]] = casedefs map {
            case cd @ CaseDef(e @ Literal(Constant(_)), EmptyTree, b) =>
              (false, List(_case(List(expr(e)), openBlock(b)).setPos(cd.pos)))
            case cd @ CaseDef(Alternative(xs), EmptyTree, b) =>
              (false, List(_case(xs map (e => expr(e)), openBlock(b)).setPos(cd.pos)))
            case cd @ CaseDef(Ident(nme.WILDCARD), EmptyTree, b) =>
              (true, openBlock(b))
            case cd =>
              c.error(cd.pos, s"Invalid javascript case expression: $cd\n  raw: ${showRaw(cd)}")
              (false, Nil)
          } groupBy (_._1) mapValues (_ map (_._2))
          val default = allCases.getOrElse(true, Nil) match {
            case List(one) =>
              "scala" sel "Some" ap List(list(one))
            case xs        =>
              if(xs.nonEmpty)
                c.error(t.pos, s"Multiple default cases found: $xs");
              "scala" sel "None"
          }
          val cases = allCases.getOrElse(false, Nil).flatten
          List(switch(expr(input), cases, default))
      }
      lazy val stTry = stPF {
        case Try(b, cs, f) =>
          val (name, cc) = cs match {
            case List(
              CaseDef(
                Apply(
                  tt @ TypeTree(),
                  List(Bind(name, Ident(nme.WILDCARD) | Typed(Ident(nme.WILDCARD), _)))
                ),
                EmptyTree,
                code
              )
            ) if tt.original.symbol.fullName == "reactive.web.js.js.Throwable" =>
              (name.decoded, statements(code))
            case _ =>
              if(cs.nonEmpty) {
                c.error(cs.head.pos, "catch block must consist of one case of the form case js.Throwable(e)")
                c.info(cs.head.pos, "found: "+cs.map(showRaw(_)), false)
              }
              ("e", Nil)
          }
          List(_try(statements(b), name, cc, statements(f)))
      }
      lazy val stThrow = stPF {
        case Throw(Apply(fun, List(e))) if "reactive.web.js.js.Throwable.apply" == fun.symbol.fullName =>
          List(_throw(expr(e)))
      }
      lazy val stFunc = stPF {
        case t @ DefDef(Mods(), name, Nil, argss, TypeTree(), body) =>
          if(argss.length != 1)
            c.error(t.pos, "Javascript methods must have one and only one argument list.")
          val args = c.Expr(list(argss.flatten.map {
            case ValDef(Mods(Flag.PARAM), paramName, _, EmptyTree) => Literal(Constant(paramName.toString))
          }))
          val b = c.Expr(list(statements(body))) //TODO
          val func = reify {
            JsAst.Function(c.literal(name.toString).splice, args.splice, JsAst.Block(b.splice))
          }.tree
          List(func)
      }
      lazy val stApply = stPF {
        case Apply(function, params) =>
          List(_apply(expr(function), params map (p => expr(p))))
      }
      lazy val stBlock = stPF {
        case Block((stmts, ret)) =>
          List(block(stmts.flatMap(s => statements(s)) ++ statements(ret): _*))
      }

      def statements(t: Tree): List[Tree] =
        stValVar orElse
          stAssign orElse
          stIf orElse
          stWhile orElse
          stDoWhile orElse
          stSwitch orElse
          stTry orElse
          stThrow orElse
          stFunc orElse stPF {
            case Literal(Constant(())) => Nil
          } applyOrElse(t, { _: Tree =>
            c.info(t.pos, s"Error converting code to javascript, input is: $body\n  raw: ${showRaw(body)}", false)
            c.error(t.pos, s"Don't know how to convert statement $t\n  raw: ${showRaw(t)}")
            List.empty
          }) map(_ setPos t.pos)

      def fixPos(t: Tree, default: Position): Tree = {
        if(t.pos == NoPosition) t.pos = default
        t.children foreach (c => fixPos(c, t.pos))
        t
      }
      val trees = (statements(body.tree) match {
        case Nil => List(block())
        case xs  => xs
      }) map (t => fixPos(t, body.tree.pos))
      val ret = c.Expr[JsAst.Statement](Block(trees.init, trees.last))
      // c.info(body.tree.pos, s"Converted from $body\n to $ret", false)  // why is this getting printed without -verbose?
      ret
    }
  }

  def javascriptImpl(c: Context)(body: c.Expr[Any]): c.Expr[JsAst.Statement] =
    new JsCompiler[c.type](c) apply body
}
