package org.hablapps.azucar

import cats.Functor

trait Signature{
  type F[_]
  val F: Functor[F]
}

trait Algebra[X]{
  val sig: Signature
  def apply(fx: sig.F[X]): X
}
