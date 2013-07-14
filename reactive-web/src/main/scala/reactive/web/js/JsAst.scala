package reactive
package web
package js

import language.experimental.macros

object js {
  class Object

  def javascript(body: Any): JsAst.Statement = macro Macros.javascriptImpl

  case class Throwable[A](value: A) extends scala.Throwable
}

sealed trait JsExprAst {
  sealed trait Expr
  sealed trait Identifier extends Expr
  sealed trait Literal extends Expr
  case class LitStr(string: String) extends Literal
  case class LitBool(bool: Boolean) extends Literal
  case class LitNum(num: BigDecimal) extends Literal
  case class SimpleIdentifier(name: String) extends Identifier
  case class Select(qualifier: Expr, name: String) extends Identifier

  def render(expr: Expr): String = expr match {
    case SimpleIdentifier(name) => name
    case Select(qual, name)     => s"${render(qual)}.$name"
    case LitStr(s)              => net.liftweb.util.Helpers.encJs(s) //TODO eliminate dependency?
    case LitBool(b)             => b.toString
    case LitNum(n)              => n.toString
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
  //TODO: for, for..in, for each..in, throw, try/catch, function, return

  val indent = new scala.util.DynamicVariable[Option[Int]](None)
  private def indentStr = indent.value.map(" " * _).getOrElse("")
  private def nl = indent.value.map(_ => "\n") getOrElse ""
  private def varsFirst(statements: Seq[Statement]) = {
    val (vars, others) = statements partition (_.isInstanceOf[Declare])
    vars ++ others
  }
  private def indented[A](b: =>A): A =
    indent.withValue(indent.value map (2 + _)) { b }
  private def indentAndRender(statement: Statement) = indented {
    indentStr + render(statement)
  }
  private def renderBlock(statements: Seq[Statement]): String =
    if(statements.isEmpty) ""
    else varsFirst(statements).map(indentAndRender).mkString(nl, ";"+nl, "")

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

  def render(statement: Statement): String = statement match {
    case Block(statements)     => "{"+renderBlock(statements)+nl+indentStr+"}"
    case Declare(name)         => s"var $name"
    case Assign(to, what)      => s"${render(to)} = ${render(what)}"
    case Apply(fun, args @ _*) => render(fun)+args.map(render).mkString("(", ",", ")")
    case If(cond, yes, no)     => s"if(${render(cond)}) ${render(yes)} else ${render(no)}"
    case While(cond, body)     => s"while(${render(cond)}) ${render(body)}"
    case DoWhile(body, cond)   => s"do ${render(body)} while(${render(cond)})"    
    case s: Switch             => renderSwitch(s)
    case Throw(e)              => s"throw ${render(e)}"
    case Try(b, n, c, f)       => "try {"+renderBlock(b)+nl+"} catch("+n+") {"+renderBlock(c)+nl+"} finally {"+renderBlock(f)+nl+"}"
  }
}
