package org.hablapps.azucar

import org.scalatest._

import scalaz.Functor

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
    type Whatever[A] = Monoid.FAlgebra[A]
  }

  it should "generate an associated ADT" in {
    trait Whatever extends Monoid.FAlgebra[Int] {
      def apply(fx: InputF[Int]): Int = fx match {
        case Mzero() => 0
        case Mappend(i1, i2) => i1 + i2
      }
    }
  }
}
