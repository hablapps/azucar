package org.hablapps.azucar

import org.scalatest._

import macros.algebra

class AlgebraTest extends FunSpec with Matchers {

  /*@algebra*/ trait Monoid[A] {
    def mzero(): A
    def mappend(a1: A, a2: A): A
  }

  trait MonoidFAlgebra {

    object Signature extends Signature{

      sealed abstract class F[_];
      case class Mzero[A]() extends F[A];
      case class Mappend[A](a1: A, a2: A) extends F[A];

      import cats.derived._;
      import functor._;
      import legacy._;
      import cats.Functor;

      val F: Functor[F] = Functor[F]
    };

    trait FAlgebra[X] extends Algebra[X]{
      val sig: Signature.type = Signature
    }

    object FAlgebra{
      def apply[X](implicit F: FAlgebra[X]) = F
    }

     import scalaz.Isomorphism.$less$tilde$greater;
     import scalaz.$tilde$greater;
     import Signature._

     val iso: <~>[Monoid, FAlgebra] = {
       final class $anon extends <~>[Monoid, FAlgebra] {
         def to: ~>[Monoid, FAlgebra] = {
           final class $anon extends ~>[Monoid, FAlgebra] {
             def apply[A](algebra: Monoid[A]): FAlgebra[A] = {
               final class $anon extends FAlgebra[A] {
                 def apply(fx: F[A]): A = fx match {
                   case Mzero() => algebra.mzero()
                   case Mappend((a1 @ _), (a2 @ _)) => algebra.mappend(a1, a2)
                 }
               };
               new $anon()
             }
           };
           new $anon()
         };
         def from: ~>[FAlgebra, Monoid] = {
           final class $anon extends ~>[FAlgebra, Monoid] {
             def apply[A](falgebra: FAlgebra[A]): Monoid[A] = {
               final class $anon extends Monoid[A] {
                 def mzero(): A = falgebra(Mzero());
                 def mappend(a1: A, a2: A): A = falgebra(Mappend(a1, a2))
               };
               new $anon()
             }
           };
           new $anon()
         }
       };
       new $anon()
     }

     implicit def fromFAlgebra[X](implicit FAlgebra: FAlgebra[X]): Monoid[X] =
      iso.from(FAlgebra)

    implicit def fromOAlgebra[X](implicit OAlgebra: Monoid[X]): FAlgebra[X] =
      iso.to(OAlgebra)

   };
   object Monoid extends MonoidFAlgebra{
     def apply[A](implicit M: Monoid[A]): Monoid[A] = M
   }

  def test(oalgebra: Monoid[Int], falgebra: Monoid.FAlgebra[Int]){
    import Monoid.Signature._

    it("zero should match"){
      oalgebra.mzero() shouldBe falgebra(Mzero())
    }

    it("mappend should match"){
      oalgebra.mappend(1,2) shouldBe falgebra(Mappend(1,2))
    }
  }

  describe("Generate F-algebras from O-algebras"){

    implicit val IntMonoidOAlgebra = new Monoid[Int] {
      def mzero() = 0
      def mappend(a1: Int, a2: Int) = a1 + a2
    }

    val IntMonoidFAlgebra: Monoid.FAlgebra[Int] = Monoid.FAlgebra[Int]
      // Monoid.iso.to(IntMonoidOAlgebra)

    test(IntMonoidOAlgebra, IntMonoidFAlgebra)
  }

  describe("Generate O-algebras from F-algebras"){
    import Monoid.Signature._

    implicit val IntMonoidFAlgebra = new Monoid.FAlgebra[Int] {
      def apply(fx: F[Int]): Int = fx match {
        case Mzero() => 0
        case Mappend(i1, i2) => i1 + i2
      }
    }

    val IntMonoidOAlgebra: Monoid[Int] = Monoid[Int]
    //   Monoid.iso.from(IntMonoidFAlgebra)

    test(IntMonoidOAlgebra, IntMonoidFAlgebra)
  }

  describe("Generate Signature"){
    import Monoid.Signature._

    it("functor evidence should work"){
      F.map(Mappend(3, 2))(_ * 2) shouldBe Mappend(6, 4)
      F.map(Mzero[Int]())(_ + 1) shouldBe Mzero()
    }
  }
}
