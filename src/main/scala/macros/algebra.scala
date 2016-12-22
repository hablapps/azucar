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

    def generate(typeClass: ClassDef) = {

      val adtCases: List[ClassDef] = {
        val typeClassMethods = typeClass.impl.children.collect {
          case m: DefDef if !m.mods.hasFlag(Flag.PRIVATE) && !m.mods.hasFlag(Flag.PROTECTED) => m
        }
        val tparamName = typeClass.tparams.head.name
        typeClassMethods.map { method => q"""
          case class ${capitalizeName(method.name)}[$tparamName](..${method.vparamss.flatten})
            extends InputF[$tparamName]
        """
        }
      }

      val adt: List[Tree] =
        q"sealed abstract class InputF[_]" :: adtCases

      val fAlias = q"type F[A] = InputF[A]"

      // TODO: how do we provide this functor evidence? shapeless?
      val fFunctor = q"implicit val F: Functor[F] = ???"

      val fAlgebra = q"""
        trait FAlgebra[X] extends Algebra[X] {
          ..$adt
          $fAlias
          $fFunctor
        }
      """

      val isoImports = List(
        q"import scalaz.Isomorphism.<~>",
        q"import scalaz.~>")

      val iso = q"""
        val iso: scalaz.Isomorphism.<~>[${typeClass.name}, FAlgebra] =
          ???
      """

      val traitName = TypeName(typeClass.name + "FAlgebra")

      val generateTrait = q"""
        trait $traitName {
          $fAlgebra
          ..$isoImports
          $iso
        }
      """

      val generateCompanion =
        q"object ${typeClass.name.toTermName} extends $traitName"

      val result = c.Expr(q"""
        $typeClass
        $generateTrait
        $generateCompanion
      """)
      trace(s"Generated algebra for '${typeClass.name}':\n" + showCode(result.tree))

      result
    }

    annottees.map(_.tree) match {
      case (typeClass: ClassDef) :: Nil => generate(typeClass)
      // case (typeClass: ClassDef) :: (companion: ModuleDef) :: Nil => ???
      case other :: Nil =>
        c.abort(c.enclosingPosition, "@algebra can only be applied to traits")
    }
  }

  def trace(s: => String) =
    c.info(c.enclosingPosition, s, false)

  def capitalizeName(name: TermName): TypeName = {
    val TermName(s) = name
    TypeName(s.capitalize)
  }
}
