package org.hablapps.azucar
package hk

import org.scalatest._

import macros.algebra

class HKAlgebraTest extends FunSpec with Matchers {

  @algebra trait Monad[F[_]] {
    def point[A](a: A): F[A]
    def bind[A,B](p: F[A])(f: A => F[B]): F[B]
  }

  object Monad {

    implicit val optionMonad = new Monad[Option] {
      def point[A](a: A) = Option(a)
      def bind[A, B](p: Option[A])(f: A => Option[B]) = p match {
        case None => None
        case Some(a) => f(a)
      }
    }

    val whatever: Boolean = true
  }

  // trait MonadFAlgebra {
  //   import scalaz.~>
  //
  //   sealed abstract class Σ[F[_],A]
  //   case class Point[F[_],A](a: A) extends Σ[F,A]
  //   case class Bind[F[_],A,B](p: F[A], f: A => F[B]) extends Σ[F,B]
  //
  //   object Σ{
  //     import scala.language.existentials
  //
  //     // experimental, not really useful for the moment
  //     import scalaz.~~>
  //     def `match`[F[_],G[_,_]](
  //       pointCase: λ[(α,β)=>β]~~>G,
  //       bindCase: λ[(α,β)=>(F[α],α=>F[β])]~~>G) =
  //       new (λ[(α,β)=>Σ[F,β]]~~>G){
  //         def apply[X,Y](s: Σ[F,Y]): G[X,Y] = s match {
  //           case point: Point[F,Y] => pointCase(point.a)
  //           case bind: Bind[F,X,Y]@unchecked => bindCase((bind.p,bind.f))
  //         }
  //       }
  //
  //     implicit val F = new Functor[Σ]{
  //       def map[F[_],G[_]](nat: F~>G) = λ[Σ[F,?]~>Σ[G,?]]{
  //         case Point(a) =>
  //           Point(a)
  //         case bind: Bind[F,_,_] =>
  //           Bind(nat(bind.p),bind.f andThen nat.apply)
  //       }
  //     }
  //   }
  //
  //   trait FAlgebra[F[_]] extends Algebra[Σ,F]{
  //     val F: Functor[Σ] = Σ.F
  //   }
  //
  //   object FAlgebra{
  //     def apply[F[_]](implicit F: FAlgebra[F]) = F
  //   }
  //
  //   implicit def fromOAlgebra[F[_]](implicit algebra: Monad[F]) =
  //     new FAlgebra[F]{
  //       val apply = λ[Σ[F,?]~>F]{
  //         case Point(a) =>
  //           algebra.point(a)
  //         case bind: Bind[F,_,_] =>
  //           algebra.bind(bind.p)(bind.f)
  //       }
  //     }
  //
  //   implicit def fromFAlgebra[F[_]](implicit falgebra: FAlgebra[F]) =
  //     new Monad[F] {
  //       def point[A](a: A): F[A] =
  //         falgebra.apply(Point(a));
  //
  //       def bind[A,B](p: F[A])(f: A => F[B]): F[B] =
  //         falgebra.apply(Bind(p, f))
  //     };
  // }
  //
  // object Monad extends MonadFAlgebra{
  //  def apply[F[_]](implicit M: Monad[F]): Monad[F] = M
  // }

  import scalaz.Id, Id._

  def test(oalgebra: Monad[Id], falgebra: Monad.FAlgebra[Id]){
    import Monad.{Point, Bind}

    it("point should match") {
      oalgebra.point(1) shouldBe falgebra.apply(Point(1))
    }

    it("bind should match") {
      oalgebra.bind[Int,Int](1)(identity) shouldBe falgebra.apply(Bind[Id,Int,Int](1, identity))
    }
  }

  describe("Generate F-algebras from O-algebras"){

    implicit val IdMonadOAlgebra = new Monad[Id] {
      def point[A](a: A) = a
      def bind[A,B](a1: A)(f: A => B) = f(a1)
    }

    val IdMonadFAlgebra: Monad.FAlgebra[Id] = Monad.FAlgebra[Id]

    test(IdMonadOAlgebra, IdMonadFAlgebra)
  }

  describe("Generate O-algebras from F-algebras") {
    import scalaz.~>
    import Monad.{Σ, Point, Bind}

    implicit val IdMonadFAlgebra = new Monad.FAlgebra[Id] {
      val apply = new (Σ[Id,?]~>Id){
        def apply[T](s: Σ[Id,T]): T = s match {
          case Point(a) => a
          case Bind(p,f) => f(p)
        }
      }
      // Don't know why it fails ...
      // λ[Σ[Id,?]~>Id]{
      //   case Point(a) => a
      //   case Bind(p, f) => f(p)
      // }
    }

    val IdMonadOAlgebra: Monad[Id] = Monad[Id]

    test(IdMonadOAlgebra, IdMonadFAlgebra)
  }

  describe("Generate Signature") {

    // it("Functor for signature should work"){
    //   import scalaz.~>
    //   import Monad.{Point, Bind, Σ}
    //
    //   val optionBind: Σ[Option,Int] =
    //     Σ.F.map(λ[Id~>Option]{Some(_)})(Bind[Id,Int,Int](1,identity))
    //
    //   optionBind should matchPattern{
    //     case Bind(Some(1),f) if f(1) == Some(1) =>
    //   }
    // }

    it("Implicit evidences should be found") {
      Functor[Monad.Σ]
    }
  }

  describe("Existing companion") {

    it("Existing encodings should prevail") {
      Monad[Option]
      Monad.whatever shouldBe true
    }
  }

  /* IO: no Fs do appear in the LHS of operators */

  import scala.io.StdIn.readLine

  @algebra trait IO[F[_]] {
    def read: F[String]
    def write(msg: String): F[Unit]
  }

  def test2(oalgebra: IO[Id], falgebra: IO.FAlgebra[Id]){
    import IO.{ Read, Write }

    it("read should match") {
      oalgebra.read shouldBe falgebra.apply(Read())
    }

    it("write should match") {
      oalgebra.write("xyz") shouldBe falgebra.apply(Write("xyz"))
    }
  }

  describe("Generate F-algebras from O-algebras (IO)"){

    implicit val IdIOOAlgebra = new IO[Id] {
      def read = "constant"
      def write(msg: String) = ()
    }

    val IdIOFAlgebra: IO.FAlgebra[Id] = IO.FAlgebra[Id]

    test2(IdIOOAlgebra, IdIOFAlgebra)
  }

  describe("Generate O-algebras from F-algebras (IO)") {
    import scalaz.~>
    import IO.{ Σ, Read, Write }

    implicit val IdIOFAlgebra = new IO.FAlgebra[Id] {
      val apply = new (Σ[Id, ?] ~> Id) {
        def apply[T](s: Σ[Id, T]): T = s match {
          case Read() => "constant"
          case Write(msg) => ()
        }
      }
      // Don't know why it fails ...
      // λ[Σ[Id,?]~>Id]{
      //   case Point(a) => a
      //   case Bind(p, f) => f(p)
      // }
    }

    val IdIOOAlgebra: IO[Id] = IO[Id]

    test2(IdIOOAlgebra, IdIOFAlgebra)
  }
}
