package org.hablapps.azucar

import cats.Functor

trait Algebra[X] {
  type F[_]
  val F: Functor[F]
  def apply(fx: F[X]): X
}
