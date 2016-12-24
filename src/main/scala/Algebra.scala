package org.hablapps.azucar

import cats.Functor

trait Signature[F[_]]{
  val F: Functor[F]
}

object Signature{
  def apply[F[_]](implicit S: Signature[F]) = S
}

trait Algebra[F[_],X]{
  val sig: Signature[F]
  def apply(fx: F[X]): X
}