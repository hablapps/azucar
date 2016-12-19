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

    def trace(s: => String) = {
      if (sys.props.get("azucar.trace").isDefined)
        c.info(c.enclosingPosition, s, false)
    }

    def generate(typeClass: ClassDef) = {

      def fAlgebra: Tree =
        q"trait FAlgebra[A]"

      def iso: Tree =
        q"def iso: scalaz.Isomorphism.<~>[${typeClass.name}, FAlgebra] = ???"

      val generateCompanion: c.Expr[Any] =
        c.Expr(q"""
          object ${typeClass.name.toTermName} {
            $fAlgebra
            $iso
          }
        """)

      val result = c.Expr(q"""
        $typeClass
        $generateCompanion
      """)
      trace(s"Generated `algebra` ${typeClass.name}:\n" + showCode(result.tree))

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
