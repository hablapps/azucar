package org.hablapps.azucar.macros

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

class algebra(
    excludeParents: List[String] = Nil,
    generateAllOps: Boolean = true) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro AlgebraMacros.generateFAlgebra
}

class AlgebraMacros(val c: Context) {
  import c.universe._

  def generateFAlgebra(annottees: c.Expr[Any]*): c.Expr[Any] = {

    def trace(s: => String) =
      c.info(c.enclosingPosition, s, false)

    def generate(typeClass: ClassDef) = {

      val adtCases: List[Tree] = List.empty

      val adt: List[Tree] = q"sealed abstract class InputF[A]" :: adtCases

      val fAlias = q"type F[A] = InputF[A]"

      val fAlgebra = q"""
        trait FAlgebra[X] extends Algebra[X] {
          ..$adt
          $fAlias
        }
      """

      val iso =
        q"def iso: scalaz.Isomorphism.<~>[${typeClass.name}, FAlgebra] = ???"

      val generateCompanion = q"""
        object ${typeClass.name.toTermName} {
          $fAlgebra
          $iso
        }
      """

      val result = c.Expr(q"""
        $typeClass
        $generateCompanion
      """)
      trace(s"Generated algebra '${typeClass.name}':\n" + showCode(result.tree))

      result
    }

    annottees.map(_.tree) match {
      case (typeClass: ClassDef) :: Nil => generate(typeClass)
      // case (typeClass: ClassDef) :: (companion: ModuleDef) :: Nil => ???
      case other :: Nil =>
        c.abort(c.enclosingPosition, "@algebra can only be applied to traits")
    }
  }
}
