package org.hablapps.azucar

import cats.Functor

trait Algebra[Σ[_], X] {
  val F: Functor[Σ]
  def apply(fx: Σ[X]): X
}
