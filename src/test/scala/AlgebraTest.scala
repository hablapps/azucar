package org.hablapps.azucar

import org.scalatest._

import macros.algebra

class AlgebraTest extends FunSpec with Matchers {

  @algebra trait Monoid[A] {
    def mzero(): A
    def mappend(a1: A, a2: A): A
  }

  // trait MonoidFAlgebra {
  //
  //   sealed abstract class Σ[_];
  //   case class Mzero[A]() extends Σ[A];
  //   case class Mappend[A](a1: A, a2: A) extends Σ[A];
  //
  //   object Σ {
  //     import cats.derived._;
  //     import functor._;
  //     import legacy._;
  //     import cats.Functor;
  //
  //     implicit val functorInstance = Functor[Σ]
  //
  //     def to[A](e: A): Σ[A] = ???
  //
  //     def from[A](s: Σ[A])(m: Monoid[A]): A = s match {
  //       case Mzero() => m.mzero()
  //       case Mappend(a1,a2) => m.mappend(a1,a2)
  //     }
  //   }
  //
  //   trait FAlgebra[X] extends Algebra[Σ, X] {
  //     import cats.instances.option._
  //     import cats.Functor
  //     val F: Functor[Σ] = Functor[Σ]
  //   }
  //
  //   object FAlgebra{
  //     def apply[X](implicit F: FAlgebra[X]) = F
  //   }
  //
  //    import scalaz.Isomorphism.$less$tilde$greater;
  //    import scalaz.$tilde$greater;
  //
  //    val iso: <~>[Monoid, FAlgebra] = {
  //      final class $anon extends <~>[Monoid, FAlgebra] {
  //        def to: ~>[Monoid, FAlgebra] = {
  //          final class $anon extends ~>[Monoid, FAlgebra] {
  //            def apply[A](algebra: Monoid[A]): FAlgebra[A] = {
  //              final class $anon extends FAlgebra[A] {
  //                def apply(fx: Σ[A]): A = fx match {
  //                  case Mzero() => algebra.mzero()
  //                  case Mappend((a1 @ _), (a2 @ _)) => algebra.mappend(a1, a2)
  //                }
  //              };
  //              new $anon()
  //            }
  //          };
  //          new $anon()
  //        };
  //        def from: ~>[FAlgebra, Monoid] = {
  //          final class $anon extends ~>[FAlgebra, Monoid] {
  //            def apply[A](falgebra: FAlgebra[A]): Monoid[A] = {
  //              final class $anon extends Monoid[A] {
  //                def mzero(): A = falgebra(Mzero());
  //                def mappend(a1: A, a2: A): A = falgebra(Mappend(a1, a2))
  //              };
  //              new $anon()
  //            }
  //          };
  //          new $anon()
  //        }
  //      };
  //      new $anon()
  //    }
  //
  //    implicit def fromFAlgebra[X](implicit FAlgebra: FAlgebra[X]): Monoid[X] =
  //     iso.from(FAlgebra)
  //
  //   implicit def fromOAlgebra[X](implicit OAlgebra: Monoid[X]): FAlgebra[X] =
  //     iso.to(OAlgebra)
  // };
  //
  // object Monoid extends MonoidFAlgebra{
  //  def apply[A](implicit M: Monoid[A]): Monoid[A] = M
  // }

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
