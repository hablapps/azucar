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

    trait Generator {

      def typeclass: ClassDef

      def existingModule: Option[ModuleDef]

      def generateImports: List[Tree]

      def generateADT: List[ClassDef]

      def generateADTCompanion: ModuleDef

      def generateFAlgebra: ClassDef

      def generateFAlgebraCompanion: ModuleDef

      def generateIso: ValDef

      def fromConversor: DefDef

      def toConversor: DefDef

      def generateMainSummoner: DefDef

      /* derived */

      val traitName: TypeName = TypeName(typeclass.name + "FAlgebra")

      val tparam: TypeDef = typeclass.tparams.head

      val typeclassMethods = typeclass.impl.children.collect {
        case m: DefDef if !m.mods.hasFlag(Flag.PRIVATE) &&
                          !m.mods.hasFlag(Flag.PROTECTED) => m
      }

      def generateMainTrait: ClassDef = q"""
        trait $traitName {
          ..$generateImports
          ..$generateADT
          $generateADTCompanion
          $generateFAlgebra
          $generateFAlgebraCompanion
          $generateIso
          $fromConversor
          $toConversor
        }
      """

      // XXX: we get the body of the existing companion by means of
      // `_.impl.body.tail`, since the first position of the body is `this`
      // constructor. Does this hold for every companion object?
      def generateMainCompanion: ModuleDef = q"""
        object ${typeclass.name.toTermName} extends $traitName {
          ..${existingModule.fold(List.empty[Tree])(_.impl.body.tail)}
          $generateMainSummoner
        }
      """
    }

    object Generator {

      def plainGenerator(
          typeclass2: ClassDef,
          existing: Option[ModuleDef] = Option.empty) = new Generator {

        def typeclass = typeclass2

        def existingModule = existing

        def generateImports = isoImports

        def generateADT = {
          val cases = typeclassMethods.map { method => q"""
            case class ${capitalize(method.name.toTypeName, TypeName(_))}[
              ${tparam.name}](
                ..${method.vparamss.flatten})
              extends Σ[${tparam.name}]
          """
          }
          q"sealed abstract class Σ[_]" :: cases
        }

        private val kittenImports = List(
          q"import cats.derived._",
          q"import functor._",
          q"import legacy._",
          q"import cats.Functor")

        private val functorIns =
          q"implicit val functorInstance = Functor[Σ]"

        def generateADTCompanion = q"""
          object Σ {
            ..$kittenImports
            $functorIns
          }
        """

        private val functorImports = List(
          q"import cats.instances.option._",
          q"import cats.Functor")

        private val fFunctor =
          q"val F: Functor[Σ] = Functor[Σ]"

        def generateFAlgebra = q"""
          trait FAlgebra[X] extends Algebra[Σ, X] {
            ..$functorImports
            $fFunctor
          }
        """

        private val fAlgSummoner =
          q"def apply[A](implicit FA: FAlgebra[A]) = FA"

        def generateFAlgebraCompanion = q"""
          object FAlgebra {
            $fAlgSummoner
          }
        """

        private val isoImports = List(
          q"import scalaz.Isomorphism.<~>",
          q"import scalaz.~>")

        private val natToIns = {
          val cases: List[CaseDef] = typeclassMethods.map {
            case DefDef(_, name, _, vparamss, _, _) => {
              val idens = vparamss.flatten.map(t => Ident(t.name))
              val binds =
                vparamss.flatten.map(t => Bind(t.name, Ident(termNames.WILDCARD)))
              val rhs = q"algebra.$name(..$idens)"
              CaseDef(q"${capitalize(name, TermName(_))}(..$binds)", EmptyTree, rhs)
            }
          }

          val match_ = Match(Ident(TermName("fx")), cases)
          q"""
            new FAlgebra[A] {
              def apply(fx: Σ[A]): A = $match_
            }
          """
        }

        private val natToDef: DefDef =q"""
          def to: ${typeclass.name} ~> FAlgebra =
            new (${typeclass.name} ~> FAlgebra) {
              def apply[A](algebra: ${typeclass.name}[A]): FAlgebra[A] =
                $natToIns
            }
        """

        private val natFromIns = {
          val defs: List[DefDef] = typeclassMethods.map {
            // XXX: removes `DEFERRED` modifier. Is this the best way to do so?
            case DefDef(_, name, tparams, vparamss, tpt, _) => {
              val args = vparamss.flatten.map(t => Ident(t.name))
              val rhs =
                q"falgebra(${capitalize(name.toTermName, TermName(_))}(..$args))"
              DefDef(Modifiers(), name, tparams, vparamss, tpt, rhs)
            }
          }
          q"""
            new ${typeclass.name}[A] {
              ..$defs
            }
          """
        }

        private val natFromDef: DefDef = q"""
          def from: FAlgebra ~> ${typeclass.name} =
            new (FAlgebra ~> ${typeclass.name}) {
              def apply[A](falgebra: FAlgebra[A]): ${typeclass.name}[A] =
                $natFromIns
            }
        """

        def generateIso: ValDef = q"""
          val iso: ${typeclass.name} <~> FAlgebra =
            new (${typeclass.name} <~> FAlgebra) {
              $natToDef
              $natFromDef
            }
        """

        def fromConversor = q"""
          implicit def fromFAlgebra[${tparam.name}](implicit
              FAlgebra: FAlgebra[${tparam.name}]): ${typeclass.name}[${tparam.name}] =
            iso.from(FAlgebra)
        """

        def toConversor = q"""
          implicit def fromOAlgebra[${tparam.name}](implicit
              OAlgebra: ${typeclass.name}[${tparam.name}]): FAlgebra[${tparam.name}] =
            iso.to(OAlgebra)
        """

        def generateMainSummoner =
          q"def apply[A](implicit ev: ${typeclass.name}[A]) = ev"
      }

      def hkGenerator(
          typeclass2: ClassDef,
          existing: Option[ModuleDef] = Option.empty) = new Generator {

        def typeclass = typeclass2

        def existingModule = existing

        def generateImports = List(q"import scalaz.~>")

        def generateADT = {
          val cases = typeclassMethods.map { method =>
            val AppliedTypeTree(_, List(arg)) = method.tpt
            q"""
              case class ${capitalize(method.name.toTypeName, TypeName(_))}[
                $tparam, ..${method.tparams}](
                  ..${method.vparamss.flatten})
                extends Σ[${tparam.name}, $arg]
            """
          }
          q"sealed abstract class Σ[F[_], X]" :: cases
        }

        // TODO: we need a HK `kittens` to bring this functionality
        // XXX: kind-projector is not working here, so I had to use manual λs!
        private val functorIns = q"""
          implicit def functorInstance = new Functor[Σ] {
            def map[G[_], H[_]](f: G ~> H)
            : ({type L1[A] = Σ[G, A]})#L1 ~> ({type L2[A] = Σ[H, A]})#L2 = ???
          }
        """

        def generateADTCompanion = q"""
          object Σ {
            $functorIns
          }
        """

        private val fFunctor =
          q"val F: Functor[Σ] = Functor[Σ]"

        def generateFAlgebra = q"""
          trait FAlgebra[${tparam.name}[_]] extends Algebra[Σ, ${tparam.name}] {
            $fFunctor
          }
        """

        private val fAlgSummoner = q"""
          def apply[${tparam.name}[_]](implicit ev: FAlgebra[${tparam.name}]) =
            ev
        """

        def generateFAlgebraCompanion = q"""
          object FAlgebra {
            $fAlgSummoner
          }
        """

        def generateIso = q"lazy val iso = ???"

        private val fromConversorRHS = {
          val cases: List[CaseDef] = typeclassMethods.map {
            case DefDef(_, name, _, vparamss, _, _) => {
              val idens = vparamss.map(_.map(t => Ident(t.name)))
              val binds =
                vparamss.flatten.map(t => Bind(t.name, Ident(termNames.WILDCARD)))
              val rhs = idens.foldLeft[Tree](q"algebra.$name")(Apply.apply)
              CaseDef(q"${capitalize(name, TermName(_))}(..$binds)", EmptyTree, rhs)
            }
          }

          // XXX: kind-projector is not working here!
          val match_ = Match(q"fa", cases)
          q"""
            {
              type ΣAux[A] = Σ[${tparam.name}, A]
              new FAlgebra[${tparam.name}] {
                val apply = new (ΣAux ~> ${tparam.name}) {
                  def apply[A](fa: ΣAux[A]): ${tparam.name}[A] = $match_
                }
              }
            }
          """
        }

        def fromConversor = q"""
          implicit def fromOAlgebra[$tparam](implicit
              algebra: ${typeclass.name}[${tparam.name}]): FAlgebra[${tparam.name}] =
            $fromConversorRHS
        """

        private val toConversorRHS = {
          val defs: List[DefDef] = typeclassMethods.map {
            case DefDef(_, name, tparams, vparamss, tpt, _) => {
              val args = vparamss.flatten.map(t => Ident(t.name))
              val rhs =
                q"falgebra.apply(${capitalize(name.toTermName, TermName(_))}(..$args))"
              DefDef(Modifiers(), name, tparams, vparamss, tpt, rhs)
            }
          }
          q"""
            new ${typeclass.name}[${tparam.name}] {
              ..$defs
            }
          """
        }

        def toConversor = q"""
          implicit def fromFAlgebra[$tparam](implicit
              falgebra: FAlgebra[${tparam.name}]): ${typeclass.name}[${tparam.name}] =
            $toConversorRHS
        """

        def generateMainSummoner = q"""
          def apply[${tparam.name}[_]](
            implicit ev: ${typeclass.name}[${tparam.name}]) = ev
        """
      }
    }

    def generate(typeclass: ClassDef, existing: Option[ModuleDef] = None) = {

      val tparam = typeclass.tparams.head

      val generator =
        if (tparam.tparams.isEmpty)
          Generator.plainGenerator(typeclass, existing)
        else
          Generator.hkGenerator(typeclass, existing)

      val result = c.Expr(q"""
        $typeclass
        ${generator.generateMainTrait}
        ${generator.generateMainCompanion}
      """)
      trace(s"Generated algebra for '${typeclass.name}':\n" + showCode(result.tree))

      result
    }

    annottees.map(_.tree) match {
      case (typeclass: ClassDef) :: Nil if typeclass.tparams.size == 1 =>
        generate(typeclass)
      case (typeclass: ClassDef) :: (companion: ModuleDef) :: Nil
          if typeclass.tparams.size == 1 =>
        generate(typeclass, Option(companion))
      case other :: Nil =>
        abort("@algebra can't be applied here")
    }
  }

  def trace(s: => String) =
    c.info(c.enclosingPosition, s, false)

  def abort(s: => String) =
    c.abort(c.enclosingPosition, s)

  def capitalize[N <: Name](name: N, builder: String => N): N =
    builder(name match {
      case TermName(s) => s.capitalize
      case TypeName(s) => s.capitalize
    })
}
