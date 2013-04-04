package reactive
package web

import scala.collection.mutable.ListBuffer
import scala.reflect.macros.Context
import scala.collection.mutable.Stack

object Templates {
  val x =
    <elem attr="value">
      <childElem attr="value" data-lift="test" />
    </elem>

  class Attr
  type X = {
    def elem: {
      def attr$eqvalue: Attr
      def childElem: {
        def attr$eqvalue: Attr
      }
    }
  }
  import scala.language.experimental.macros
  import scala.language.dynamics

  def printf(format: String, params: Any*): Unit = macro printf_impl

  def printf_impl(c: Context)(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._
    val Literal(Constant(s_format: String)) = format.tree
    val evals = ListBuffer[ValDef]()
    def precompute(value: Tree, tpe: Type): Ident = {
      val freshName = newTermName(c.fresh("eval$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
      Ident(freshName)
    }
    val paramsStack = Stack[Tree](params map (_.tree): _*)
    def refs(params: List[Tree], formats: List[String]): List[Tree] = (params, formats) match {
      case (p :: ps, "%d" :: fs) => precompute(p, typeOf[Int]) :: refs(ps, fs)
      case (p :: ps, "%s" :: fs) => precompute(p, typeOf[String]) :: refs(ps, fs)
      case (ps, "%%" :: fs)      => Literal(Constant("%")) :: refs(ps, fs)
      case (ps, s :: fs)         => Literal(Constant(s)) :: refs(ps, fs)
      case (Nil, Nil)            => Nil
    }
    val statements = evals ++
      refs(params.toList.map(_.tree), s_format.split("(?<=%[\\w%])|(?=%[\\w%])").toList).map(
        ref => reify(print(c.Expr[Any](ref).splice)).tree
      )
    statements foreach println
    c.Expr[Unit](Block(statements.toList, Literal (Constant(()))))
  }

  //printf("%s", "Hello")

  import scala.xml._

  class Template[F] extends Dynamic {

    def applyDynamicNamed(name: String)(args: (String, NodeSeq => NodeSeq)*): Any =
    macro applyDynamicNamedImpl[F]

    // def applyDynamicNamed(name: String)(args: (String, Any)*) {
    //   println("applyDynamicNamed: " + name + args.map(x => x._1+" = " + x._2).mkString("(",",",")"))
    // }
  }

  def replace(ns: NodeSeq, s: String, f: NodeSeq=>NodeSeq) = {
    def node(n: Node): NodeSeq = n match {
      case e: Elem =>
        val e1 = Elem(
          e.prefix,
          e.label,
          e.attributes,
          e.scope,
          e.minimizeEmpty,
          e.child flatMap node: _*)
        e.attribute("data-lift") match {
          case Some(Text(`s`)) => f(e1)
          case _               => e1
        }
      case n => n
    }
    ns flatMap node
  }

  def applyDynamicNamedImpl[F]
    (c: Context)
    (name: c.Expr[String])
    (args: c.Expr[(String, NodeSeq => NodeSeq)]*)(implicit ftt: c.WeakTypeTag[F]) : c.Expr[NodeSeq] =
  {
    import c.universe._
 
    // Check that the method name is "apply".
    name.tree match {
      case Literal(Constant(n)) =>
        if (n != "apply")
          c.error(name.tree.pos, s"Expected 'apply', got '$n'")
      case _ =>
        c.error(name.tree.pos, "Unexpected name structure error")
    }
    
    val filename = ftt.tpe.declarations.toList match {
      case List(ms: MethodSymbol) => ms.name.decoded
    }

    //TODO other parsers
    val x = scala.xml.XML.loadFile(filename)

    // println((reify(scala.xml.Elem("","",???,???,false, List(???, ???): _*))))
    println(reify(List(Nil: _*)))
    println

    def xexpr(n: Node): Tree = n match {
      case e: Elem =>
        val Elem(pre, lbl, atr, sc, ch @ _*) = e
        val me = e.minimizeEmpty
        val funcName = atr.get("data-lift").map(_.text)
        def atrExp = atr match {
          case _ => //TODO
            Select(
              Select(
                Ident("scala"),
                newTermName("xml")
              ),
              newTermName("Null")
            )
        }
        def scExp = sc match {
          case _ => // TODO
            Select(
              Select(
                Ident("scala"),
                newTermName("xml")
              ),
              newTermName("TopScope")
            )
        }
        val args1 =
          List(
            Literal(Constant("")),
            Literal(Constant(lbl)),
            atrExp,
            scExp,
            Literal(Constant(me))
          )
        val argsChild = ch map { n => xexpr(n) }
        val ret = Apply(
          Select(
            Select(
              Select(
                Ident("scala"),
                newTermName("xml")
              ),
              newTermName("Elem")
            ),
            newTermName("apply")
          ),
          args1 ++ argsChild
        )
        val nsFunc = args.map(_.tree).collect {
          case a @ Apply(_, List(Literal(Constant(n)), v)) if Some(n) == funcName =>
            v
        }.headOption
        nsFunc match {
          case Some(f) =>
            Apply(Select(f, newTermName("apply")), List(ret))
          case _ => ret
        }
      case Text(t) =>
        Apply(
          Select(
            Select(
              Select(
                Ident("scala"),
                newTermName("xml")
              ),
              newTermName("Text")
            ),
            newTermName("apply")
          ),
          List(Literal(Constant(t)))
        )
    }

    val xdef = xexpr(x)

    println(xdef)

    c.Expr[NodeSeq](xdef)
  }

}
