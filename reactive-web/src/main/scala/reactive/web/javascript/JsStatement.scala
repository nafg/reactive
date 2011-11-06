package reactive
package web
package javascript

import JsTypes._

object Javascript {
  def apply(f: => Unit): Unit = {
    val js = JsStatement.inScope(f)
    js foreach { s => Reactions.queue(s) }
  }
}

/**
 * A scala representation of a javascript statement.
 * On instantiation, puts itself on the current JsStatement stack.
 */
sealed trait JsStatement {
  /**
   * A list of JsStatements that if they are
   * on the top of the JsStatement stack should be replaced.
   */
  def toReplace: List[JsStatement]

  for (e <- toReplace)
    if (e eq JsStatement.peek)
      JsStatement.pop
    else println(JsStatement.peek+" != "+e)

  JsStatement.push(this)
}
/**
 * Maintains a thread-local stack of statement blocks
 */
object JsStatement {
  private def renderMatch(m: Match[_]) = "case "+m.against.render+": "+m.code.map(render).mkString(";\n")+";\nbreak;"
  def render(statement: JsStatement): String = statement match {
    case i: If.If                        => "if("+i.cond.render+") "+render(i.body)
    case e: If.Elseable#Else             => render(e.outer)+" else "+render(e.body)
    case ei: If.Elseable#ElseIf          => render(ei.outer)+" else if("+ei.cond.render+") "+render(ei.body)
    case s: Apply1[_, _]                 => s.render
    case s: ApplyProxyMethod[_]          => s.render
    case b: Block                        => "{"+b.body.map(JsStatement.render).mkString(";\n")+"}"
    case w: While.While                  => "while("+w.cond.render+") "+render(w.body)
    case dw: Do.DoWhile                  => "do "+render(dw.body)+" while("+dw.cond.render+")"
    case s: Switch.Switch[_]             => "switch("+s.input.render+") {"+s.matches.map(renderMatch).mkString("\n")+"}"
    case v: JsVar[_]                     => "var "+v.ident.name
    case a: JsVar[_]#Assignment          => a.ident.name+"="+a.init.render
    case f: For.For                      => "for("+f.init.map(render).mkString(",")+";"+f.cond.render+";"+f.inc.map(render).mkString(",")+") "+render(f.body)
    case fi: ForInable[_]#ForIn          => "for("+render(fi.v)+" in "+fi.exp.render+") "+render(fi.body)
    case fei: ForEachInable[_]#ForEachIn => "for each("+render(fei.v)+" in "+fei.exp.render+") "+render(fei.body)
    case Throw(e)                        => "throw "+e.render
    case t: Try.Try                      => "try "+render(t.body)
    case c: Try.Try#Catch                => render(c.outer)+" catch("+c.v.ident.name+") "+render(c.body)
    case f: Try.Finallyable#Finally      => render(f.outer)+" finally "+render(f.body)
  }

  /**
   * A dynamically-scoped stack of blocks of JsStatements
   */
  val stack = new scala.util.DynamicVariable[List[List[JsStatement]]](List(Nil))
  /**
   * The top JsStatement block
   */
  def currentScope: List[JsStatement] = stack.value.head
  /**
   * Sets the top JsStatement block
   */
  def currentScope_=(ss: List[JsStatement]) = stack.value = ss :: stack.value.tail
  /**
   * Returns true if there is no other statement block on the stack
   */
  def bottomScope = stack.value.tail.isEmpty
  /**
   * Evaluates p in a new scope, and returns the top of the stack
   * (JsStatements pushed during evaluation of p)
   */
  def inScope(p: => Unit): List[JsStatement] = stack.withValue(Nil :: stack.value){
    p
    currentScope.reverse
  }

  def push(s: JsStatement) = {
    currentScope ::= s
  }
  def pop: JsStatement = {
    val ret = currentScope.head
    currentScope = currentScope.tail
    ret
  }
  def peek = currentScope.head
  def replace(pf: PartialFunction[JsStatement, JsStatement]) =
    if (pf.isDefinedAt(peek)) push(pf(pop))
}

final private class Block(block: => Unit) extends JsStatement {
  lazy val body = JsStatement.inScope(block)
  def toReplace = Nil
}

sealed class HasBody(block: => Unit) {
  private[javascript] lazy val body = new Block(block)
}

case class Apply1[P <: JsAny, +R <: JsAny](f: $[P =|> R], arg0: $[P]) extends JsStatement with JsExp[R] {
  def toReplace = List(arg0).collect{ case s: JsStatement => s }
  def render = f.render+"("+arg0.render+")"
}
class ApplyProxyMethod[+R <: JsAny](ident: String, method: java.lang.reflect.Method, clazz: Class[_], args: Seq[_]) extends JsStatement with JsExp[R] {
  def toReplace = args.toList.collect{ case s: JsStatement => s }
  private def hasField = try {
    clazz.getField(method.getName)
    true
  } catch {
    case _: NoSuchFieldException =>
      false
  }
  def render =
    ident+"."+method.getName + {
      if (args.isEmpty && hasField)
        ""
      else if (method.getParameterTypes.forall(classOf[JsExp[_]].isAssignableFrom))
        args.map(_.asInstanceOf[JsExp[_]].render).mkString("(", ",", ")")
      else
        args.map(_.toString).mkString("(", ",", ")")
    }
}

object If {
  trait Elseable { this: JsStatement =>
    def Else(block: => Unit) = new Else(block)
    def ElseIf(cond: $[JsBoolean])(block: => Unit) = new ElseIf(cond)(block)
    class ElseIf(private[javascript] val cond: $[JsBoolean])(block: => Unit) extends HasBody(block) with Elseable with JsStatement {
      private[reactive] lazy val outer = Elseable.this
      def toReplace = List(outer)
    }
    class Else(block: => Unit) extends HasBody(block) with JsStatement {
      private[javascript] lazy val outer = Elseable.this
      def toReplace = List(outer)
    }
  }
  def apply(cond: $[JsBoolean])(block: => Unit): JsStatement with Elseable = new If(cond, block)
  private[javascript] class If(val cond: $[JsBoolean], block: => Unit) extends HasBody(block) with Elseable with JsStatement {
    def toReplace = Nil
  }
}

object While {
  private[javascript] class While(val cond: $[JsBoolean])(block: => Unit) extends HasBody(block) with JsStatement {
    def toReplace = Nil
  }
  def apply(cond: $[JsBoolean])(block: => Unit) = new While(cond)(block)
}

object Do {
  private[javascript] class DoWhile(block: => Unit)(val cond: $[JsBoolean]) extends HasBody(block) with JsStatement {
    def toReplace = Nil
  }
  def apply(block: => Unit) = new {
    def While(cond: $[JsBoolean]) = new DoWhile(block)(cond)
  }
}

sealed trait Match[+T <: JsAny] {
  def against: $[T]
  def code: List[JsStatement]
}
class Matchable[+T <: JsAny](against: $[T]) { matchable =>
  def :>(code: => Unit) = {
    lazy val block = JsStatement.inScope(code)
    new Match[T] {
      def against = matchable.against
      def code = block
    }
  }
}

object Switch {
  private[javascript] class Switch[T <: JsAny](val input: $[T])(val matches: Match[T]*) extends JsStatement {
    def toReplace = Nil
  }
  def apply[T <: JsAny](input: $[T])(matches: Match[T]*) = new Switch(input)(matches: _*)
}

class JsVar[T <: JsAny] extends NamedIdent[T] with JsStatement {
  def toReplace = Nil
  class Assignment(private[javascript] val init: $[T]) extends JsStatement {
    val ident = JsVar.this.ident
    def toReplace = List(init) collect { case s: JsStatement => s }
  }
  def :=(exp: $[T]) = new Assignment(exp)
}

object JsVar {
  /**
   * Create a JsVar with a fresh name
   */
  def apply[T <: JsAny]()(implicit p: Page) = new JsVar[T] {
    override val ident = Symbol("x$"+p.nextNumber)
  }
}

object For {
  class For(
    private[javascript] val init: Seq[JsVar[_]#Assignment],
    private[javascript] val cond: $[JsBoolean],
    private[javascript] val inc: Seq[JsVar[_]#Assignment])(block: => Unit) extends HasBody(block) with JsStatement {
    def toReplace = inc ++ init toList
  }
  def apply(init: Seq[JsVar[_]#Assignment], cond: $[JsBoolean], inc: Seq[JsVar[_]#Assignment])(block: => Unit) = new For(init, cond, inc)(block)
}

case class ForInable[T <: JsAny](exp: JsExp[JsArray[T]]) {
  class ForIn(private[javascript] val v: JsVar[JsNumber], private[javascript] val exp: JsExp[JsArray[T]])(block: => Unit) extends HasBody(block) with JsStatement {
    def toReplace = List(v)
  }
  def foreach(f: JsIdent[JsNumber] => Unit)(implicit p: Page) = {
    val v = JsVar[JsNumber]()
    new ForIn(v, exp)(f(v))
  }
}
case class ForEachInable[T <: JsAny](exp: JsExp[JsArray[T]]) {
  class ForEachIn(private[javascript] val v: JsVar[T], private[javascript] val exp: JsExp[JsArray[T]])(block: => Unit) extends HasBody(block) with JsStatement {
    def toReplace = List(v)
  }
  def foreach(f: JsIdent[T] => Unit)(implicit p: Page) = {
    val v = JsVar[T]()
    new ForEachIn(v, exp)(f(v))
  }
}

case class Throw[T <: JsAny](exp: JsExp[T]) extends JsStatement {
  def toReplace = Nil
}

object Try {
  trait Finallyable { this: JsStatement =>
    def Finally(block: => Unit) = new Finally(block)
    class Finally(block: => Unit) extends HasBody(block) with JsStatement {
      private[javascript] lazy val outer = Finallyable.this
      def toReplace = List(outer)
    }
  }
  def apply(block: => Unit) = new Try(block)
  private[javascript] class Try(block: => Unit) extends HasBody(block) with Finallyable with JsStatement {
    def toReplace = Nil
    def Catch(b: JsIdent[JsAny] => Unit) = {
      val v = JsVar[JsAny]()
      new Catch(v)(b(v))
    }
    class Catch(private[javascript] val v: JsVar[JsAny])(block: => Unit) extends HasBody(block) with Finallyable with JsStatement {
      private[javascript] lazy val outer = Try.this
      def toReplace = List(v, outer)
    }
  }
}
