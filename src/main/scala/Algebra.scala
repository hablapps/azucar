package org.hablapps.azucar

import scalaz.Functor

trait Algebra[X] {
  type F[_]
  implicit val F: Functor[F]
  def apply(fx: F[X]): X
}
