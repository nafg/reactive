package reactive
package web
package js

import scala.reflect.macros.Context

object Macros {
  def javascriptImpl(c: Context)(body: c.Expr[Any]): c.Expr[JsAst.Statement] = {
    import c.universe._

    implicit class treeOps(tree: Tree) {
      def sel(name: TermName) = Select(tree, name)
      def ap(args: List[Tree]) = Apply(tree, args)
      def tAp(tArgs: List[Tree]) = TypeApply(tree, tArgs)
    }
    implicit class stringOps(s: String) {
      def sel(name: TermName) = Ident(s) sel name
    }

    def jsAst = reify(reactive.web.js.JsAst).tree

    def block(trees: Tree*) =
      jsAst sel "Block" ap List(list(trees.toList))
    def str(s: String) = jsAst sel "LitStr" ap List(Literal(Constant(s)))
    def bool(b: Boolean) = jsAst sel "LitBool" ap List(Literal(Constant(b)))
    def num(n: Tree) = jsAst sel "LitNum" ap List(n)
    def declare(n: String) = jsAst sel "Declare" ap List(Literal(Constant(n)))
    def assign(to: Tree, what: Tree) = jsAst sel "Assign" ap List(to, what)
    def simpleIdent(n: String) = jsAst sel "SimpleIdentifier" ap List(Literal(Constant(n)))
    def select(q: Tree, n: String) = jsAst sel "Select" ap List(q, Literal(Constant(n)))
    def _apply(fun: Tree, args: List[Tree]) = jsAst sel "Apply" ap fun +: args
    def _if(cond: Tree, yes: Tree, no: Tree) = jsAst sel "If" ap List(cond, yes, no)
    def _while(cond: Tree, body: Tree) = jsAst sel "While" ap List(cond, body)
    def doWhile(body: Tree, cond: Tree) = jsAst sel "DoWhile" ap List(body, cond)
    def switch(in: Tree, cases: List[Tree], default: Tree) =
      jsAst sel "Switch" ap List(
        in,
        list(cases),
        default
      )
    def _case(test: List[Tree], st: List[Tree]) = jsAst sel "Case" ap List(list(test), list(st))

    def list(els: List[Tree]) = "scala" sel "List" ap els

    def trace(in: Tree)(out: Tree) = {
      println("in: "+in)
      println("out: "+out)
      out
    }

    def expr(t: Tree): Tree = (t match {
      case Literal(Constant(s: String)) =>
        str(s)
      case Literal(Constant(b: Boolean)) =>
        bool(b)
      case n @ Literal(Constant((_: Int) | (_: Double) | (_: Long) | (_: BigDecimal))) =>
        num(n)
      case Select(left, right) =>
        if(!(left.tpe <:< typeOf[js.Object]))
          c.error(left.pos, "Can only select members of a js.Object")
        select(expr(left), right.toString)
      case Ident(name) =>
        simpleIdent(name.toString)
      case _ =>
        c.info(t.pos, s"Error converting code to javascript, input is: $body\n  raw: ${showRaw(body)}", false)
        c.abort(t.pos, s"Don't know how to convert expression $t\n raw: ${showRaw(t)}")
    }).setPos(t.pos)

    def statements(t: Tree): List[Tree] = (t match {
      case ValDef(m, name, TypeTree(), e) if m == Modifiers() || m == Modifiers(Flag.MUTABLE) =>
        List(
          declare(name.toString),
          assign(simpleIdent(name.toString), expr(e))
        )
      case Assign(name, e) =>
        List(assign(simpleIdent(name.toString), expr(e)))
      case If(cond, yes, no) =>
        List(
          _if(expr(cond), block(statements(yes): _*), block(statements(no): _*))
        )
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
      case Match(input, casedefs) =>
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
            c.error(cd.pos, s"Invalid case expression: $cd\n  raw: ${showRaw(cd)}")
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
      case Apply(function, params) =>
        List(_apply(expr(function), params map (p => expr(p))))
      case Literal(Constant(())) =>
        Nil
      case Block((stmts, ret)) =>
        List(block(stmts.flatMap(s => statements(s)) ++ statements(ret): _*))
      case _ =>
        c.info(t.pos, s"Error converting code to javascript, input is: $body\n  raw: ${showRaw(body)}", false)
        c.error(t.pos, s"Don't know how to convert statement $t\n  raw: ${showRaw(t)}")
        Nil
    }).map(_ setPos t.pos)
  
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
