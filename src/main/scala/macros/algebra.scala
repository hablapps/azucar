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

      val typeClassMethods = typeClass.impl.children.collect {
        case m: DefDef if !m.mods.hasFlag(Flag.PRIVATE) && !m.mods.hasFlag(Flag.PROTECTED) => m
      }

      val tparamName = typeClass.tparams.head.name

      val adtCases: List[ClassDef] = {
        typeClassMethods.map { method => q"""
          case class ${capitalizeName(method.name.toTypeName)}[$tparamName](
              ..${method.vparamss.flatten})
            extends InputF[$tparamName]
        """
        }
      }

      val adt: List[Tree] =
        q"sealed abstract class InputF[_]" :: adtCases

      val fAlias = q"type F[A] = InputF[A]"

      // TODO: how do we provide functor evidences? maybe shapeless?
      val fFunctor = q"implicit lazy val F: Functor[F] = ???"

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

      val natToIns = {
        val cases: List[CaseDef] = typeClassMethods.map {
          case DefDef(_, name, _, vparamss, _, _) => {
            val idens = vparamss.flatten.map(t => Ident(t.name))
            val binds =
              vparamss.flatten.map(t => Bind(t.name, Ident(termNames.WILDCARD)))
            val rhs = q"algebra.$name(..$idens)"
            CaseDef(q"${capitalizeName(name)}(..$binds)", EmptyTree, rhs)
          }
        }

        val match_ = Match(Ident(TermName("fx")), cases)
        q"""
          new FAlgebra[A] {
            def apply(fx: InputF[A]): A = $match_
          }
        """
      }

      val natToDef: DefDef =q"""
        def to: ${typeClass.name} ~> FAlgebra =
          new (${typeClass.name} ~> FAlgebra) {
            def apply[A](algebra: ${typeClass.name}[A]): FAlgebra[A] =
              $natToIns
          }
      """

      val natFromIns = {
        val defs: List[DefDef] = typeClassMethods.map {
          // XXX: removes `DEFERRED` modifier. Is this the best way to do so?
          case DefDef(_, name, tparams, vparamss, tpt, _) => {
            val args = vparamss.flatten.map(t => Ident(t.name))
            val rhs =
              q"falgebra(falgebra.${capitalizeName(name.toTermName)}(..$args))"
            DefDef(Modifiers(), name, tparams, vparamss, tpt, rhs)
          }
        }
        q"""
          new ${typeClass.name}[A] {
            ..$defs
          }
        """
    }

      val natFromDef: DefDef = q"""
        def from: FAlgebra ~> ${typeClass.name} =
          new (FAlgebra ~> ${typeClass.name}) {
            def apply[A](falgebra: FAlgebra[A]): ${typeClass.name}[A] =
              $natFromIns
          }
      """

      val isoVal: ValDef = q"""
        val iso: ${typeClass.name} <~> FAlgebra =
          new (${typeClass.name} <~> FAlgebra) {
            $natToDef
            $natFromDef
          }
      """

      val traitName = TypeName(typeClass.name + "FAlgebra")

      val generateTrait = q"""
        trait $traitName {
          $fAlgebra
          ..$isoImports
          $isoVal
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

  def capitalizeName(name: TermName): TermName = {
    val TermName(s) = name
    TermName(s.capitalize)
  }

  def capitalizeName(name: TypeName): TypeName = {
    val TypeName(s) = name
    TypeName(s.capitalize)
  }
}
