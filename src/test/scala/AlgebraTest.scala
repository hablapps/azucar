package org.hablapps.azucar

import org.scalatest._

import macros.algebra

class AlgebraTest extends FlatSpec with Matchers {

  import cats.derived._, functor._, legacy._
  import cats.Functor

  case class Person[A](name: String, a: A)

  Functor[Person]

  @algebra trait Monoid[A] {
    def mzero(): A
    def mappend(a1: A, a2: A): A
  }

  val intMonoid = new Monoid[Int] {
    def mzero() = 0
    def mappend(a1: Int, a2: Int) = a1 + a2
  }

  "Azucar" should "generate a type FAlgebra[A]" in {
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

  it should "generate an isomorphism" in {
    val falgebra = Monoid.iso.to(intMonoid)
    import falgebra._
    falgebra(Mappend(3, 2)) shouldBe 5
    falgebra(Mzero()) shouldBe 0
  }

  it should "generate a functor evidence for InputF" in {
    val falgebra = Monoid.iso.to(intMonoid)
    import falgebra._
    F.map(Mappend(3, 2))(_ * 2) shouldBe Mappend(6, 4)
    F.map(Mzero[Int]())(_ + 1) shouldBe Mzero()
  }
}
