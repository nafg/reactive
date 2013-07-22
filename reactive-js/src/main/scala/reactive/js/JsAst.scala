package reactive.js

import language.experimental.macros

object js {
  class Object
  def Object[A](elements: (String, A)*): Object = js.stub

  def stub = sys.error("Javascript stub, should not be called from scala code")

  case class Array[A](xs: A*) extends Object {
    def push(x: A): Unit = stub
    def shift(): A = stub
    def foreach(f: Int => Unit): Unit = stub
    def apply(idx: Int): A = stub
  }

  def javascript(body: Any): JsAst.Statement = macro Macros.javascriptImpl

  case class Throwable[A](value: A) extends scala.Throwable
}

sealed trait JsExprAst { this: JsAst.type =>
  sealed trait Expr
  sealed trait Identifier extends Expr
  sealed trait Literal extends Expr
  case class LitStr(string: String) extends Literal
  case class LitBool(bool: Boolean) extends Literal
  case class LitNum(num: BigDecimal) extends Literal
  case class LitArray(xs: List[Expr]) extends Literal
  case class LitObject(xs: List[(String, Expr)]) extends Literal
  case class LitFunction(args: List[String], body: Block) extends Literal
  case class SimpleIdentifier(name: String) extends Identifier
  case class Select(qualifier: Expr, name: String) extends Identifier
  case class Index(obj: Expr, index: Expr) extends Expr
  case class BinOp(left: Expr, operator: String, right: Expr) extends Expr

  /**
   * Based on net.liftweb.util.StringHelpers#encJs
   */
  def renderString(in: String): String = in match {
    case null => "null"
    case _ =>
      def escUnicode(in: Char) = {
        val ret = Integer.toString(in.toInt, 16)
        "\\u" + ("0000".substring(ret.length)) + ret
      }

      val len = in.length
      val sb = new java.lang.StringBuilder(len * 2) append '"'
      var pos = 0
      while (pos < len) {
        in.charAt(pos) match {
          case c @ ('"' | '\\') => sb append '\\' append c
          case '\n' => sb append "\\n"
          case '\r' => sb append "\\r"
          case '\t' => sb append "\\t"
          case c if c < ' ' || c == ']' || c.toInt >= 127 => sb append escUnicode(c)
          case c => sb append c
        }
        pos += 1
      }
      sb.append('"').toString
  }

  def render(expr: Expr): String = expr match {
    case SimpleIdentifier(name)  => name
    case Select(qual, name)      => s"${render(qual)}.$name"
    case LitStr(s)               => renderString(s)
    case LitBool(b)              => b.toString
    case LitNum(n)               => n.toString
    case LitArray(xs)            => xs.map(render).mkString("[", ",", "]")
    case Index(obj, idx)         => render(obj) + "[" + render(idx) + "]"
    case LitFunction(args, body) => "function(" + args.mkString(",") + ") " + render(body)
    case LitObject(xs)           => xs.map{ case (k, v) => render(LitStr(k)) + ": " + render(v) }.mkString("{", ",", "}")
    case BinOp(left, op, right)  => s"(${render(left)} $op ${render(right)})"
    case Apply(func, args @ _*)  => render(func) + args.map(render).mkString("(", ",", ")")
  }
}

object JsAst extends JsExprAst {
  sealed trait Statement
  case class Block(statements: List[Statement]) extends Statement
  case class Apply(function: Expr, args: Expr*) extends Statement with Expr
  case class If(cond: Expr, yesBranch: Statement, noBranch: Statement) extends Statement
  case class While(cond: Expr, body: Statement) extends Statement
  case class DoWhile(body: Statement, cond: Expr) extends Statement
  case class Switch(input: Expr, cases: List[Case], default: Option[List[Statement]]) extends Statement
  case class Case(test: List[Expr], body: List[Statement])
  case object Break extends Statement
  case class Assign(to: Identifier, what: Expr) extends Statement
  case class Declare(name: String) extends Statement
  case class Throw(e: Expr) extends Statement
  case class Try(block: List[Statement], catchName: String, catcher: List[Statement], finalizer: List[Statement]) extends Statement
  case class Function(name: String, args: List[String], body: Block) extends Statement
  case class Return(e: Expr) extends Statement
  case class For(varName: String, start: Expr, end: Expr, step: Expr, inclusive: Boolean, statement: Statement) extends Statement
  case class ForIn(varName: String, target: Expr, statement: Statement) extends Statement

  val indent = new scala.util.DynamicVariable[Option[Int]](None)
  private def indentStr = indent.value.map(" " * _).getOrElse("")
  private def nl = indent.value.map(_ => "\n") getOrElse ""
  private def varsFirst(statements: Seq[Statement]) = {
    val (vars, others) = statements partition (_.isInstanceOf[Declare])
    vars ++ others
  }
  private def indented[A](b: => A): A =
    indent.withValue(indent.value map (2 + _)) { b }
  private def indentAndRender(statement: Statement) = indented {
    indentStr + render(statement)
  }

  private def renderBlock(statements: Seq[Statement]): String =
    if (statements.isEmpty) ""
    else varsFirst(statements).map(indentAndRender).mkString(nl, ";" + nl, "")
  private def renderSwitch(s: Switch) =
    s"switch(${render(s.input)}) {" +
      indented {
        s.cases.map{
          case Case(test, body) => s"$nl${indentStr}case ${test map render mkString ", "}: " + renderBlock(body) + ";"
        }.mkString +
          s.default.map{ body =>
            s"$nl${indentStr}default: " + renderBlock(body) + ";"
          }.getOrElse("")
      } + s"$nl$indentStr}"
  private def renderFor(f: For): String = {
    import f._
    val cmp = step match {
      case LitNum(n) if n < 0 => if (inclusive) ">=" else ">"
      case _                  => if (inclusive) "<=" else "<"
    }
    s"for(var $varName=${render(start)}; $varName $cmp ${render(end)}; $varName += ${render(step)}) ${render(statement)}"
  }

  def render(statement: Statement): String = statement match {
    case Block(statements)     => "{" + renderBlock(statements) + nl + indentStr + "}"
    case Declare(name)         => s"var $name"
    case Assign(to, what)      => s"${render(to)} = ${render(what)}"
    case Apply(fun, args @ _*) => render(fun) + args.map(render).mkString("(", ",", ")")
    case If(cond, yes, no)     => s"if(${render(cond)}) ${render(yes)} else ${render(no)}"
    case While(cond, body)     => s"while(${render(cond)}) ${render(body)}"
    case DoWhile(body, cond)   => s"do ${render(body)} while(${render(cond)})"
    case s: Switch             => renderSwitch(s)
    case Throw(e)              => s"throw ${render(e)}"
    case Try(b, n, c, f)       => "try {" + renderBlock(b) + nl + "} catch(" + n + ") {" + renderBlock(c) + nl + "} finally {" + renderBlock(f) + nl + "}"
    case Function(n, args, b)  => s"function $n(${args.mkString(",")}) ${render(b)}"
    case Return(exp)           => "return " + render(exp)
    case f: For                => renderFor(f)
    case ForIn(nm, target, st) => s"for($nm in ${render(target)}) ${render(st)}"
  }
}
