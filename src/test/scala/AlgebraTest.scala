package org.hablapps.azucar

import org.scalatest._

import macros.algebra

class AlgebraTest extends FlatSpec with Matchers {

  @algebra trait Monoid[A] {
    def mzero: A
    def mappend(a1: A, a2: A): A
  }

  "Azucar" should "generate an isomorphism" in {
    Monoid.iso
  }

  it should "generate a type FAlgebra[A]" in {
    type F[A] = Monoid.FAlgebra[A]
  }
}
