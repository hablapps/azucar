package org.hablapps.azucar

import org.scalatest._

import macros.algebra

class AlgebraTest extends FunSpec with Matchers {

  @algebra trait Monoid[A] {
    def mzero(): A
    def mappend(a1: A, a2: A): A
  }

  object Monoid {

    def instance[A](mzero2: A)(mappend2: (A, A) => A) = new Monoid[A] {
      def mzero() = mzero2
      def mappend(a1: A, a2: A) = mappend2(a1, a2)
    }

    implicit def stringInstance: Monoid[String] = instance("")(_ + _)

    val whatever: Boolean = true
  }

  def test(oalgebra: Monoid[Int], falgebra: Monoid.FAlgebra[Int]){
    import Monoid.{Mzero, Mappend}

    it("zero should match") {
      oalgebra.mzero() shouldBe falgebra(Mzero())
    }

    it("mappend should match") {
      oalgebra.mappend(1, 2) shouldBe falgebra(Mappend(1, 2))
    }

    // TODO: macros to/from for Step/Σ
    // it("macros"){
    //   Monoid.Σ.to(oalgebra.mzero()) shouldBe Mzero()
    //   Monoid.Σ.to(oalgebra.mappend(1,2)) shouldBe Mappend(1,2)
    //
    //   Monoid.Σ.from(Mzero())(oalgebra) shouldBe oalgebra.mzero()
    //   Monoid.Σ.from(Mappend(1,2))(oalgebra) shouldBe oalgebra.mappend(1,2)
    // }
  }

  describe("Generate F-algebras from O-algebras"){

    implicit val IntMonoidOAlgebra = new Monoid[Int] {
      def mzero() = 0
      def mappend(a1: Int, a2: Int) = a1 + a2
    }

    val IntMonoidFAlgebra: Monoid.FAlgebra[Int] =
      Monoid.FAlgebra[Int]

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
      scalaz.Functor[Monoid.Σ]
    }
  }

  describe("Existing companion") {

    it("Existing encodings should prevail") {
      Monoid[String]
      Monoid.whatever shouldBe true
    }
  }

  describe("Catamorphism for free") {
    import scalaz.syntax.id._

    import matryoshka._
    import matryoshka.implicits._
    import matryoshka.data.Mu

    it("Given an algebra, folds the whole structure") {
      import Monoid.{ Σ, Mzero, Mappend, FAlgebra }

      implicit def monoidCorecursive[T](implicit
          T: Corecursive.Aux[T, Σ]): Monoid[T] =
        Monoid.instance(Mzero[T]().embed)(Mappend[T](_, _).embed)

      def expr[A](implicit M: Monoid[A]): A =
        M.mappend(M.mzero, M.mzero)

      expr[Mu[Σ]].cata(FAlgebra[String]) shouldBe ""
    }

    // https://github.com/slamdata/matryoshka#introduction
    @algebra trait Expr[A] {
      def num(value: Long): A
      def mul(l: A, r: A): A
    }

    it("Given matryoshka's tutorial expression, eval it!") {
      import Expr.{ Σ, Num, Mul, FAlgebra }

      implicit def eval = new Expr[Long] {
        def num(value: Long) = value
        def mul(l: Long, r: Long) = l * r
      }

      implicit def exprCorecursive[T](implicit
          T: Corecursive.Aux[T, Σ]): Expr[T] =
        new Expr[T] {
          def num(value: Long) = Num[T](value).embed
          def mul(l: T, r: T) = Mul[T](l, r).embed
        }

      def expr[A](implicit E: Expr[A]): A =
        E.mul(E.num(2), E.mul(E.num(3), E.num(4)))

      expr[Mu[Σ]].cata(FAlgebra[Long]) shouldBe 24

      // or alternatively

      expr[Long] shouldBe 24
    }
  }
}
