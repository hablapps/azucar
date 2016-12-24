package org.hablapps.azucar

import org.scalatest._

import macros.algebra

class AlgebraTest extends FunSpec with Matchers {

  @algebra trait Monoid[A] {
    def mzero(): A
    def mappend(a1: A, a2: A): A
  }

  def test(oalgebra: Monoid[Int], falgebra: Monoid.FAlgebra[Int]){
    import falgebra.{Mzero, Mappend}

    it("zero should match"){
      oalgebra.mzero() shouldBe falgebra(Mzero())
    }

    it("mappend should match"){
      oalgebra.mappend(1,2) shouldBe falgebra(Mappend(1,2))
    }
  }

  describe("Generate F-algebras from O-algebras"){

    val IntMonoidOAlgebra = new Monoid[Int] {
      def mzero() = 0
      def mappend(a1: Int, a2: Int) = a1 + a2
    }

    val IntMonoidFAlgebra: Monoid.FAlgebra[Int] =
      Monoid.iso.to(IntMonoidOAlgebra)

    test(IntMonoidOAlgebra, IntMonoidFAlgebra)
  }

  describe("Generate O-algebras from F-algebras"){

    val IntMonoidFAlgebra = new Monoid.FAlgebra[Int] {
      def apply(fx: InputF[Int]): Int = fx match {
        case Mzero() => 0
        case Mappend(i1, i2) => i1 + i2
      }
    }

    val IntMonoidOAlgebra: Monoid[Int] =
      Monoid.iso.from(IntMonoidFAlgebra)

    test(IntMonoidOAlgebra, IntMonoidFAlgebra)


    it("functor evidence should work"){
      // import cats.derived._, functor._, legacy._
      // import cats.Functor

      import IntMonoidFAlgebra._

      F.map(Mappend(3, 2))(_ * 2) shouldBe Mappend(6, 4)
      F.map(Mzero[Int]())(_ + 1) shouldBe Mzero()
    }
  }

}
