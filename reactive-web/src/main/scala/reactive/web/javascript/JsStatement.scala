package reactive
package web
package javascript

import JsTypes._

/**
 * Statements written with the JsStatement DSL, within a Javascript { ... } block
 * will be sent to the browser.
 */
object Javascript {
  def apply[A](f: => A)(implicit page: Page): A = Reactions.inAnyScope(page){
    val (ret, js) = JsStatement.inScope(f)
    js foreach { s => Reactions.queue(s) }
    ret
  }
}

/**
 * Wrap a scala function in Ajax(...) to get a javascript function that will
 * call the scala function via ajax.
 * @example {{{
 *   val func = Ajax{x: String => println("Got "+x+"!")}
 *   Javascript {
 *     func("10")
 *   }
 * }}}
 */
object Ajax {
  def apply(f: () => Unit)(implicit observing: Observing, page: Page): JsExp[JsFunction0[JsVoid]] = {
    val id = page.nextNumber
    page.ajaxEvents ?>> {
      case (_id, _) if _id == id.toString =>
        f()
    }
    //TODO reactive should be a JsStub, so this could be scala code
    JsRaw(
      "(function(){reactive.queueAjax("+id+")();reactive.doAjax()})"
    )
  }
  def apply[J <: JsAny, S](f: S => Unit)(implicit fromJs: FromJs[J, S], observing: Observing, page: Page): $[J =|> JsVoid] = {
    val id = page.nextNumber
    page.ajaxEvents ?>> {
      case (_id, json) if _id == id.toString =>
        f(fromJs.parser(json))
    }
    //TODO reactive should be a JsStub, so this could be scala code
    JsRaw(
      "(function(arg0){reactive.queueAjax("+id+")("+
        JsExp.render(fromJs.encoder(JsRaw[J]("arg0")))+
        ");reactive.doAjax()})"
    )
  }
  def apply[J1 <: JsAny, S1, J2 <: JsAny, S2](f: (S1, S2) => Unit)(implicit fromJs1: FromJs[J1, S1], fromJs2: FromJs[J2, S2], observing: Observing, page: Page): JsExp[JsFunction2[J1, J2, JsVoid]] = {
    val id = page.nextNumber
    page.ajaxEvents ?>> {
      case (_id, net.liftweb.json.JArray(List(json1, json2))) if _id == id.toString =>
        f(fromJs1.parser(json1), fromJs2.parser(json2))
    }
    //TODO reactive should be a JsStub, so this could be scala code
    JsRaw(
      "(function(arg0,arg1){reactive.queueAjax("+id+")("+
        JsExp.render(List(fromJs1.encoder(JsRaw[J1]("arg0")), fromJs2.encoder(JsRaw[J2]("arg1"))))+
        ");reactive.doAjax()})"
    )
  }
}

/**
 * A scala representation of a javascript statement.
 * On instantiation, puts itself on the current JsStatement stack.
 */
sealed trait JsStatement {
  /**
   * A list of JsStatements that if they are
   * on the top of the JsStatement stack should be replaced
   * by this statement (generally because it renders them itself).
   */
  def toReplace: List[JsStatement]

  for (e <- toReplace; head <- JsStatement.peek)
    if (e eq head)
      JsStatement.pop
    else println("'"+JsStatement.render(head)+"' is the top of the stack, not '"+JsStatement.render(e)+
      "', when applying toReplace for '"+JsStatement.render(this)+"'")

  JsStatement.push(this)
}
/**
 * Maintains a thread-local stack of statement blocks
 */
object JsStatement {
  /**
   * Some to pretty print at the given indent, None for compressed output
   */
  val indent = new scala.util.DynamicVariable[Option[Int]](None)
  def indentStr = indent.value.map(" " * _).getOrElse("")
  def nl = indent.value.map(_ => "\n") getOrElse ""
  private val renderMatch: Match[_ <: JsAny] => String = m =>
    "case "+JsExp.render(m.against)+":"+nl + m.code.map(indentAndRender).mkString(";"+nl)+";"+nl
  private def varsFirst(stmts: Seq[JsStatement]) = {
    val (vars, others) = stmts partition (_.isInstanceOf[JsVar[_]])
    vars ++ others
  }
  def render(statement: JsStatement) = indent.value match {
    case Some(i) => renderImpl(statement)
    case _       => renderImpl(statement)
  }
  private def indentAndRender(statement: JsStatement) = indent.withValue(indent.value map (2+))(indentStr + render(statement))
  private[javascript] def renderBlock(statements: List[JsStatement]): String = if (statements.isEmpty) "{}" else varsFirst(statements).map(indentAndRender).mkString("{"+nl, ";"+nl, nl + indentStr+"}")
  private def renderImpl(statement: JsStatement): String = statement match {
    case i: If.If                        => "if("+JsExp.render(i.cond)+") "+renderImpl(i.body)
    case e: If.Elseable#Else             => render(e.outer)+" else "+render(e.body)
    case ei: If.Elseable#ElseIf          => render(ei.outer)+" else if("+JsExp.render(ei.cond)+") "+render(ei.body)
    case s: Apply[_]                     => s.render
    case s: ApplyProxyMethod[_]          => s.render
    case b: Block                        => renderBlock(b.body)
    case w: While.While                  => "while("+JsExp.render(w.cond)+") "+render(w.body)
    case dw: Do.DoWhile                  => "do "+render(dw.body)+" while("+JsExp.render(dw.cond)+")"
    case s: Switch.Switch[_]             => "switch("+JsExp.render(s.input)+") {"+nl + s.matches.map(renderMatch).mkString+"}"
    case b: Break                        => "break"
    case v: JsVar[_]                     => "var "+v.ident.name
    case a: Assignable[_]#Assignment     => a.ident+"="+JsExp.render(a.init)
    case f: For.For                      => "for("+f.init.map(renderImpl _).mkString(",")+";"+JsExp.render(f.cond)+";"+f.inc.map(renderImpl _).mkString(",")+") "+render(f.body)
    case fi: ForInable[_]#ForIn          => "for("+render(fi.v)+" in "+JsExp.render(fi.exp)+") "+render(fi.body)
    case fei: ForEachInable[_]#ForEachIn => "for each("+render(fei.v)+" in "+JsExp.render(fei.exp)+") "+render(fei.body)
    case Throw(e)                        => "throw "+JsExp.render(e)
    case t: Try.Try                      => "try "+render(t.body)
    case c: Try.Try#Catch                => render(c.outer)+" catch("+c.v.ident.name+") "+render(c.body)
    case f: Try.Finallyable#Finally      => render(f.outer)+" finally "+render(f.body)
    case Return(e)                       => "return "+JsExp.render(e)
    case f: Function[_]                  => "function "+f.ident.name+"(arg0)"+render(f.body)
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
  def inScope[A](p: => A): (A, List[JsStatement]) = stack.withValue(Nil :: stack.value){
    (p, currentScope.reverse)
  }

  def push(s: JsStatement) = {
    currentScope ::= s
  }
  def pop: JsStatement = {
    val ret = currentScope.head
    currentScope = currentScope.tail
    ret
  }
  def peek = currentScope.headOption
}

final private class Block(block: => Unit) extends JsStatement {
  lazy val (_, body) = JsStatement.inScope(block)
  def toReplace = Nil
}

sealed class HasBody(block: => Unit) {
  private[javascript] lazy val body = new Block(block)
}

case class Apply[+R <: JsAny](f: JsExp[_ <: JsAny], args: JsExp[_ <: JsAny]*) extends JsStatement with JsExp[R] {
  def toReplace = args.toList.collect { case s: JsStatement => s }
  def render = JsExp.render(f) + args.map(JsExp.render).mkString("(", ",", ")")
}

class ProxyField[R <: JsAny](ident: String, name: String) extends Assignable[R] {
  def render = ident+"."+name
}
class ApplyProxyMethod[R <: JsAny](ident: String, method: java.lang.reflect.Method, args: Seq[Any], oldToReplace: List[JsStatement]) extends JsStatement with Assignable[R] {
  // TODO detect varargs better
  lazy val flat =
    if (method.getParameterTypes.toList == List(classOf[scala.collection.Seq[_]]) && args.length == 1)
      args.collect{ case x: Seq[Any] => x }.head
    else
      args

  lazy val toReplace = oldToReplace ++ flat.toList.collect{ case s: JsStatement => s }

  lazy val render = ident+"."+method.getName + {
    flat map {
      case x: JsExp[_] => JsExp render x
      case null        => "null"
      case x           => x.toString
    } mkString ("(", ",", ")")
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
    lazy val (_, block) = JsStatement.inScope(code)
    new Match[T] {
      def against = matchable.against
      def code = block :+ Break
    }
  }
}

class Break extends JsStatement {
  def toReplace = Nil
}

object Switch {
  private[javascript] class Switch[T <: JsAny](val input: $[T])(val matches: Match[T]*) extends JsStatement {
    def toReplace = Nil
  }
  def apply[T <: JsAny](input: $[T])(matches: Match[T]*) = new Switch(input)(matches: _*)
}

trait Assignable[T <: JsAny] extends JsExp[T] {
  def :=(exp: $[T]) = new Assignment(exp)
  class Assignment(private[javascript] val init: $[T]) extends JsStatement {
    lazy val ident: String = JsExp.render(Assignable.this)
    def toReplace = List(init) collect { case s: JsStatement => s }
  }
}
class JsVar[T <: JsAny] extends NamedIdent[T] with Assignable[T] with JsStatement {
  def toReplace = Nil
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
    private[javascript] val init: Seq[JsVar[_ <: JsAny]#Assignment],
    private[javascript] val cond: $[JsBoolean],
    private[javascript] val inc: Seq[JsVar[_ <: JsAny]#Assignment])(block: => Unit) extends HasBody(block) with JsStatement {
    def toReplace = inc ++ init toList
  }
  def apply(init: Seq[JsVar[_ <: JsAny]#Assignment], cond: $[JsBoolean], inc: Seq[JsVar[_ <: JsAny]#Assignment])(block: => Unit) = new For(init, cond, inc)(block)
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
    def Catch(b: JsIdent[JsAny] => Unit)(implicit page: Page) = {
      val v = JsVar[JsAny]()
      new Catch(v)(b(v))
    }
    class Catch(private[javascript] val v: JsVar[JsAny])(block: => Unit) extends HasBody(block) with Finallyable with JsStatement {
      private[javascript] lazy val outer = Try.this
      def toReplace = List(v, outer)
    }
  }
}

class Function[P <: JsAny](val capt: $[P] => Unit) extends NamedIdent[P =|> JsAny] with JsStatement {
  private[javascript] lazy val body = new Block(capt('arg0 $))
  def toReplace = Nil
}
object Function {
  def apply[P <: JsAny](capt: $[P] => Unit)(implicit p: Page) = new Function[P](capt) {
    override val ident = Symbol("f$"+p.nextNumber)
  }
}

case class Return[T <: JsAny](exp: JsExp[T] = JsRaw("")) extends JsStatement {
  def toReplace = Nil
}
