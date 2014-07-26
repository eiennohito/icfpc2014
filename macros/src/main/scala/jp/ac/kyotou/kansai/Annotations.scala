package jp.ac.kyotou.kansai

import scala.language.experimental.macros

import scala.annotation.StaticAnnotation

/**
 * @author eiennohito
 * @since 2014-07-26
 */
class gccCode extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro gccCodeMacro.macroImpl
}

case class MacroException(msg: String) extends RuntimeException(msg)

object gccCodeMacro {
  import scala.reflect.macros.whitebox.Context
  def macroImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val trees = annottees.map(_.tree).toList

    if (trees.length != 2) {
      c.error(c.enclosingPosition, "annotated class should have companion object")
    }

    println(trees)

    val clz :: companion :: Nil = trees

    val content = trees.head match {
      case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        stats
    }

    def transformExprTree(tree: Tree): ExprAst = {
      tree match {
        case q"$left.$func(..$args)" => Application(func.encodedName.toString,
          transformExprTree(left),
          args.collect {
            case x: Tree => transformExprTree(x)
            case x => throw new MacroException(s"unsupported tree value $x")
          })
        case q"$func(..$args)" =>
        case x => throw new MacroException(s"unsupported expression pattern $x")
      }
    }

    def transformDef(name: TermName, params: List[List[ValDef]], body: Tree) = {
      body.collect {
        case q"val $nm = ${value: Int}" => AssignConst(nm.decodedName.toString, value)
        case expr @ q"$left.$func(..$args)" => Expression(transformExprTree(expr))
        case expr @ q"$func(..$args)" => Expression(transformExprTree(expr))
      }
    }

    content.map {
      case q"$mods def $name[..$tparams](...$paramss): $tpt = $expr" => println(s"$name $expr $tpt")
      case x => println(s"unsupported structure: $x")
    }

    val newCompanion = companion match {
      case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        val newstats = stats.collect {
          case q"$mods val asts: Map[String, Any] = ???" => q"""$mods val asts: Map[String, Any] = Map("test" -> 10)"""
          case x => x
        }
        q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$newstats }"
      case x =>
        c.error(c.enclosingPosition, s"companion object is not an object: $x")
        return c.Expr(EmptyTree)
    }

    c.Expr[Any](Block(List(clz, newCompanion), Literal(Constant(()))))
  }

  def transformBody = ???
}
