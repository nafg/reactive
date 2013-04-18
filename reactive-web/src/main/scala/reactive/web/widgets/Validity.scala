package reactive
package web
package widgets

import scala.xml.NodeSeq
import net.liftweb.common.Full
import net.liftweb.common.Box
import net.liftweb.common.Failure
import net.liftweb.util.Helpers._
import scala.xml.Text

import reactive.Observing
import reactive.web.PropertyVar


object Validity {
  trait FailureMessage[-From, -To, Msg] {
    def apply(value: From): Msg
  }
  implicit def stringFailureMessage[From, To: Manifest]: FailureMessage[From, To, String] = new FailureMessage[From, To, String] {
    def apply(value: From) = "Not a valid "+manifest[To].erasure.getSimpleName+": "+value
  }
  implicit def box2StrValidity[A](box: Box[A])(implicit fm: FailureMessage[String, A, String], m: Manifest[A]): Validity[A, String] = box match {
    case Full(a)            => Valid(a)
    case Failure(msg, _, _) => Invalid(fm(msg))
    case _                  => Invalid(fm("Empty box"))
  }
}


sealed trait Validity[+A, +Msg] {
  def isValid: Boolean
  def messages: Seq[Msg]
  def value: A

  def map[B](f: A => B): Validity[B, Msg]
  def flatMap[B, M2 >: Msg](f: A => Validity[B, M2]): Validity[B, M2]
  def filter[M2 >: Msg](p: A => Boolean)(implicit fm: Validity.FailureMessage[A, A, M2]): Validity[A, M2]

  def warnIf[M2 >: Msg](p: A => Boolean, msg: => M2): Validity[A, M2]
}
case class Valid[+A](value: A) extends Validity[A, Nothing] {
  def isValid = true
  def messages = throw new UnsupportedOperationException("message of Valid")

  def map[B](f: A => B): Validity[B, Nothing] = Valid(f(value))
  def flatMap[B, M2 >: Nothing](f: A => Validity[B, M2]): Validity[B, M2] = f(value)
  def filter[M2 >: Nothing](p: A => Boolean)(implicit fm: Validity.FailureMessage[A, A, M2]): Validity[A, M2] = if (p(value)) this else Invalid(fm(value))

  def warnIf[M2 >: Nothing](p: A => Boolean, msg: => M2): Validity[A, M2] = if (p(value)) Warning(value, msg) else this
}
case class Warning[+A, +Msg](value: A, messages: Msg*) extends Validity[A, Msg] {
  def isValid = false

  def map[B](f: A => B): Validity[B, Msg] = Warning(f(value), messages: _*)
  def flatMap[B, M2 >: Msg](f: A => Validity[B, M2]): Validity[B, M2] = f(value) match {
    case Valid(x)              => Warning(x, messages: _*)
    case Invalid(msgs @ _*)    => Invalid(messages ++ msgs: _*)
    case Warning(x, msgs @ _*) => Warning(x, messages ++ msgs: _*)
  }
  def filter[M2 >: Msg](p: A => Boolean)(implicit fm: Validity.FailureMessage[A, A, M2]): Validity[A, M2] = if (p(value)) this else Invalid(fm(value))

  def warnIf[M2 >: Msg](p: A => Boolean, msg: => M2): Validity[A, M2] = if (p(value)) Warning(value, messages :+ msg: _*) else this
}
case class Invalid[+Msg](messages: Msg*) extends Validity[Nothing, Msg] {
  def isValid = false
  def value = throw new UnsupportedOperationException("value of Invalid")

  def map[B](f: Nothing => B): Validity[B, Msg] = this
  def flatMap[B, M2 >: Msg](f: Nothing => Validity[B, M2]): Validity[B, M2] = this
  def filter[M2 >: Msg](p: Nothing => Boolean)(implicit fm: Validity.FailureMessage[Nothing, Nothing, M2]): Validity[Nothing, M2] = this

  def warnIf[M2 >: Msg](p: Nothing => Boolean, msg: => M2): Validity[Nothing, M2] = this
}


object NodeSeqValidity {
  import Validity._
  implicit def nodeSeqFailureMessage[From, To](implicit m: Manifest[To], stringMessage: FailureMessage[From, To, String]): FailureMessage[From, To, NodeSeq] = new FailureMessage[From, To, NodeSeq] {
    def apply(value: From) = Text(stringMessage(value))
  }
  implicit def box2NSValidity[A](box: Box[A])(implicit fm: FailureMessage[String, A, NodeSeq], m: Manifest[A]): Validity[A, NodeSeq] = box match {
    case Full(a)            => Valid(a)
    case Failure(msg, _, _) => Invalid(fm(msg))
    case _                  => Invalid(fm("Empty box"))
  }
}


case class ValLens[A, B, Msg](out: A => B, in: B => Validity[A, Msg])

object ValLens {
  import Validity._
  implicit def idLens[A, Msg]: ValLens[A, A, Msg] = ValLens(identity, Valid(_))
  implicit def intString[Msg](implicit fm: FailureMessage[String, Int, Msg]): ValLens[Int, String, Msg] = ValLens(_.toString, s => try { Valid(s.toInt) } catch { case e: NumberFormatException => Invalid(fm(s)) })
  implicit def doubleString[Msg](implicit fm: FailureMessage[String, Double, Msg]): ValLens[Double, String, Msg] = ValLens(_.toString, s => try { Valid(s.toDouble) } catch { case e: NumberFormatException => Invalid(fm(s)) })
}

/**
 * Allows to wrap an Editor, returning an Editor, that adds a visual of the validity state. 
 */
object ValidityCue {
  class ValidityCueConfig {
    /**
     * The css selector for the element to add a class for the validity state
     */
    def validitySelector = ".val"
    def messageSelector = ".msg"
    def editorSelector = ".edit"
    def validClass: Option[String] = None
    def invalidClass = "error"
    def warningClass = "warning"
    def validityClass[A](v: Validity[A, NodeSeq]): Option[String] = v match {
      case _: Valid[_]      => validClass
      case _: Warning[_, _] => Some(warningClass)
      case _: Invalid[_]    => Some(invalidClass)
    }
    def message[A](v: Validity[A, NodeSeq]): NodeSeq => NodeSeq = v match {
      case Valid(_) => _ => NodeSeq.Empty
      case v        => _ => v.messages.map(ns => <p>{ ns }</p>)
    }
  }
  def validityCue[A](editor: Editor[A])(implicit observing: Observing, page: Page, config: ValidityCueConfig): Editor[A] = {
    import config._
    new Editor(
      validitySelector #> PropertyVar.appendClass(editor.value map validityClass) andThen
        messageSelector #> reactive.web.Cell(editor.value map message) andThen
        editorSelector #> editor,
      editor.value,
      editor.pageIds
    )
  }
}
