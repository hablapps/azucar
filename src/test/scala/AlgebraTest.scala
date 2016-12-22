package org.hablapps.azucar

import org.scalatest._

import scalaz.Functor

import macros.algebra

class AlgebraTest extends FlatSpec with Matchers {

  @algebra trait Monoid[A] {
    def mzero: A
    def mappend(a1: A, a2: A): A
  }

  /*** Iso to be generated ***/
  import scalaz.Isomorphism.<~>
  import scalaz.~>
  import Monoid.FAlgebra
  val iso: Monoid <~> FAlgebra = new (Monoid <~> FAlgebra) {
    def to: Monoid ~> FAlgebra = new (Monoid ~> FAlgebra) {
      def apply[A](monoid: Monoid[A]): FAlgebra[A] = new FAlgebra[A] {
        def apply(fx: InputF[A]): A = fx match {
          case Mzero() => monoid.mzero
          case Mappend(a1, a2) => monoid.mappend(a1, a2)
        }
      }
    }
    def from: FAlgebra ~> Monoid = new (FAlgebra ~> Monoid) {
      def apply[A](falgebra: FAlgebra[A]): Monoid[A] = new Monoid[A] {
        def mzero = falgebra(falgebra.Mzero())
        def mappend(a1: A, a2: A) = falgebra(falgebra.Mappend(a1, a2))
      }
    }
  }
  /*** End of iso ***/

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
