package reactive

import scala.language.higherKinds
trait Applicative[M[_]] {
  def ap[A, B](f: M[A => B])(a: M[A]): M[B]
  def point[A](a: => A): M[A]
}

object Applicative {
  trait ApType {
    type FS[+Z]
  }
  object ApType {
    trait One[A] extends ApType {
      type FS[+Z] = A => Z
    }
    trait More[A, N <: ApType] extends ApType {
      type FS[+Z] = A => N#FS[Z]
    }
  }

  trait ApplicativeBuilder[M[_], AT <: ApType] {
    implicit def m: Applicative[M]
    def ap[Z](f: M[AT#FS[Z]]): M[Z]
    def apPoint[Z](f: AT#FS[Z]): M[Z] = ap(m.point(f))
    def :@:[B](b: M[B]): ApplicativeBuilder[M, ApType.More[B, AT]] = new ApplicativeBuilder.More[M, B, AT](b, this)
  }
  object ApplicativeBuilder {
    class One[M[_], A](val in: M[A])(implicit val m: Applicative[M]) extends ApplicativeBuilder[M, ApType.One[A]] {
      override def ap[Z](f: M[A => Z]): M[Z] = m.ap(f)(in)
    }
    class More[M[_], A, N <: ApType](val a: M[A], val next: ApplicativeBuilder[M, N])(implicit val m: Applicative[M]) extends ApplicativeBuilder[M, ApType.More[A, N]] {
      override def ap[Z](f: M[A => N#FS[Z]]): M[Z] = next.ap(m.ap(f)(a))
    }
  }
}
