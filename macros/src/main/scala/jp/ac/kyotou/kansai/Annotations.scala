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

object TreeLifters {
  import jp.ac.kyotou.{kansai => ast}
  import scala.reflect.runtime.universe._

  def liftStructure(x: ast.StructureAst): Tree = x match {
    case ast.FunctionDefiniton(name, args, code) => q"ast.FunctionDefiniton($name, $args, ${code.map(liftCode)})"
  }

  def liftCode(x: ast.CodeAst): Tree = x match {
    case ast.Assign(name, result) => q"ast.Assign($name, ${liftExpr(result)})"
    case ast.Expression(expr) => q"ast.Expression(${liftExpr(expr)})"
    case ast.Return(expr) => q"ast.Return(${liftExpr(expr)})"
  }

  def liftExpr (x: ast.ExprAst): Tree = x match {
    case ast.Literal(i) => q"ast.Literal($i)"
    case ast.FunCall(name, args) => q"ast.FunCall($name, ${args.map(liftExpr)})"
    case ast.Plus(left, right) => q"ast.Plus(${liftExpr(left)}, ${liftExpr(right)})"
    case ast.Minus(left, right) => q"ast.Minus(${liftExpr(left)}, ${liftExpr(right)})"
    case ast.Multiply(left, right) => q"ast.Multiply(${liftExpr(left)}, ${liftExpr(right)})"
    case ast.Divide(left, right) => q"ast.Divide(${liftExpr(left)}, ${liftExpr(right)})"
    case ast.Reference(name) => q"ast.Reference($name)"
    case ast.Application(funcName, context, args) => q"ast.Application($funcName, ${liftExpr(context)}, ${args.map(liftExpr)})"
  }
}

object gccCodeMacro {
  import scala.reflect.macros.whitebox.Context

  import jp.ac.kyotou.{kansai => ast}
  import TreeLifters._

  def macroImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val trees = annottees.map(_.tree).toList

    if (trees.length != 2) {
      c.error(c.enclosingPosition, "annotated class should have companion object")
    }

    println(trees)

    val clz :: companion :: Nil = trees

    val content: List[Tree] = clz match {
      case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        stats
    }

    def transformExprTree(tree: Tree): ExprAst = {
      tree match {
        case q"$left.$func(..$args)" => ast.Application(func.encodedName.toString,
          transformExprTree(left),
          args.collect {
            case x: Tree => transformExprTree(x)
            case x => throw new MacroException(s"unsupported tree value $x")
          })
        case q"${func: TermName}(..$args)" => ast.FunCall(func.encodedName.toString,
          args.collect {
            case x: Tree => transformExprTree(x)
            case x => throw new MacroException(s"unsupported tree value $x")
          }
        )
        case q"${lit: Int}" => ast.Literal(lit)
        case q"${ref: TermName}" => ast.Reference(ref.decodedName.toString)
        case x => throw new MacroException(s"unsupported expression pattern $x")
      }
    }

    def transformDef(name: TermName, params: List[List[ValDef]], body: Tree): FunctionDefiniton = {
      val bodyast = body.collect {
        case q"val $nm = ${value: Int}" => ast.Assign(nm.encodedName.toString, ast.Literal(value))
        case expr @ q"$left.$func(..$args)" => ast.Expression(transformExprTree(expr))
        case expr @ q"$func(..$args)" => ast.Expression(transformExprTree(expr))
        case q"return $expr" => ast.Return(transformExprTree(expr))
        case x => throw new MacroException(s"unsupported Scala statement: $x")
      }
      FunctionDefiniton(name.encodedName.toString,
          params.flatMap(x => x.map(extractArgName)),
          bodyast)
    }

    def extractArgName(arg: ValDef): String = {
      arg match {
        case q"val $name: $tpe = $expr" => name.decodedName.toString
        case x => throw new MacroException(s"unsupported function argument definition $x")
      }
    }

    val structureObjs = content.flatMap {
      case q"$mods def $name[..$tparams](...$paramss): $tpt = $expr" => transformDef(name, paramss, expr) :: Nil
      case x => throw new MacroException(s"unsupported structure: $x")
    }

    val structure = structureObjs.map(x => x.name -> liftStructure(x))

    def untupling(x: (String, scala.reflect.runtime.universe.Tree)) = {
      q"(${x._1}, ${x._2.asInstanceOf[Tree]})"
    }

    val newCompanion = companion match {
      case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        val newstats = stats.collect {
          case q"$mods val asts: Map[String, StructureAst] = ???" => q"""$mods val asts: Map[String, StructureAst] = Map(..${structure.map(x => untupling(x))})"""
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