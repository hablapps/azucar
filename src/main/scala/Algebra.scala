package org.hablapps.azucar

import cats.Functor

trait Signature[Σ[_]]{
  val F: Functor[Σ]
}

object Signature{
  def apply[Σ[_]](implicit S: Signature[Σ]) = S
}

trait Algebra[Σ[_],X]{
  val sig: Signature[Σ]
  def apply(fx: Σ[X]): X
}