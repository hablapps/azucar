package org.hablapps.azucar.hk

import scalaz.~>

trait Functor[F[_[_],_]]{
  def map[G[_], H[_]](f: G ~> H): F[G,?] ~> F[H,?]
}

object Functor{
  def apply[F[_[_],_]](implicit F: Functor[F]) = F
}

trait Algebra[F[_[_],_], E[_]]{
  val F: Functor[F]
  val apply: F[E,?] ~> E
}

