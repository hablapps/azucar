package org.hablapps.azucar

trait Algebra[F[_], X] {
  type F[_]
  implicit val F: Functor[F]
  def apply(fx: F[X]): X
}
