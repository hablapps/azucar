package org.hablapps.azucar

import org.scalatest._

import macros.algebra

class AlgebraTest extends FunSpec with Matchers {

  @algebra trait Monoid[A] {
    def mzero(): A
    def mappend(a1: A, a2: A): A
  }

  def test(oalgebra: Monoid[Int], falgebra: Monoid.FAlgebra[Int]) {
    import Monoid.{ Mzero, Mappend }

    it("zero should match") {
      oalgebra.mzero() shouldBe falgebra(Mzero())
    }

    it("mappend should match") {
      oalgebra.mappend(1, 2) shouldBe falgebra(Mappend(1, 2))
    }
  }

  describe("Generate F-algebras from O-algebras"){

    implicit val IntMonoidOAlgebra = new Monoid[Int] {
      def mzero() = 0
      def mappend(a1: Int, a2: Int) = a1 + a2
    }

    val IntMonoidFAlgebra: Monoid.FAlgebra[Int] = Monoid.FAlgebra[Int]

    test(IntMonoidOAlgebra, IntMonoidFAlgebra)
  }

  describe("Generate O-algebras from F-algebras") {
    import Monoid.{Σ, Mzero, Mappend}

    implicit val IntMonoidFAlgebra = new Monoid.FAlgebra[Int] {
      def apply(fx: Σ[Int]): Int = fx match {
        case Mzero() => 0
        case Mappend(i1, i2) => i1 + i2
      }
    }

    val IntMonoidOAlgebra: Monoid[Int] = Monoid[Int]

    test(IntMonoidOAlgebra, IntMonoidFAlgebra)
  }

  describe("Generate Signature") {

    it("Functor for signature should work"){
      import Monoid.{ Mzero, Mappend }, Monoid.Σ. { functorInstance => F }

      F.map(Mappend(3, 2))(_ * 2) shouldBe Mappend(6, 4)
      F.map(Mzero[Int]())(_ + 1) shouldBe Mzero()
    }

    it("Implicit evidences should be found") {
      cats.Functor[Monoid.Σ]
    }
  }
}
