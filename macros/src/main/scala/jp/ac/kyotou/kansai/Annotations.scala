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
}

object gccCodeMacro {
  import scala.reflect.macros.whitebox.Context

  import jp.ac.kyotou.{kansai => ast}
  import TreeLifters._

  def macroImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val trees = annottees.map(_.tree).toList

    def liftStructure(x: StructureAst): Tree = x match {
      case ast.FunctionDefiniton(name, args, code) => q"jp.ac.kyotou.kansai.FunctionDefiniton($name, $args, ${code.map(y => liftCode(y))})"
    }

    def liftCode(x: CodeAst): Tree = x match {
      case ast.Assign(name, result) => q"jp.ac.kyotou.kansai.Assign($name, ${liftExpr(result)})"
      case ast.Expression(expr) => q"jp.ac.kyotou.kansai.Expression(${liftExpr(expr)})"
      case ast.Return(expr) => q"jp.ac.kyotou.kansai.Return(${liftExpr(expr)})"
      case ast.Block(expr) => q"jp.ac.kyotou.kansai.Block(${expr.map(y => liftCode(y))})"
    }

    def liftExpr (x: ExprAst): Tree = x match {
      case ast.Literal(i) => q"jp.ac.kyotou.kansai.Literal($i)"
      case ast.FunCall(name, args) => q"jp.ac.kyotou.kansai.FunCall($name, ${args.map(y => liftExpr(y))})"
      case ast.Plus(left, right) => q"jp.ac.kyotou.kansai.Plus(${liftExpr(left)}, ${liftExpr(right)})"
      case ast.Minus(left, right) => q"jp.ac.kyotou.kansai.Minus(${liftExpr(left)}, ${liftExpr(right)})"
      case ast.Multiply(left, right) => q"jp.ac.kyotou.kansai.Multiply(${liftExpr(left)}, ${liftExpr(right)})"
      case ast.Divide(left, right) => q"jp.ac.kyotou.kansai.Divide(${liftExpr(left)}, ${liftExpr(right)})"
      case ast.Reference(name) => q"jp.ac.kyotou.kansai.Reference($name)"
      case ast.Application(funcName, context, args) => q"jp.ac.kyotou.kansai.Application($funcName, ${liftExpr(context)}, ${args.map(y => liftExpr(y))})"
    }

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

    def transformStatement(statement: Tree): CodeAst = {
      println(s"transform statement: $statement")
      statement match {
        case q"val $nm: $tp = ${value: Int}" => ast.Assign(nm.encodedName.toString, ast.Literal(value))
        case q"val $nm: $tp = $expr" => ast.Assign(nm.encodedName.toString, transformExprTree(expr))
        case expr @ q"$left.$func(..$args)" => ast.Expression(transformExprTree(expr))
        case expr @ q"$func(..$args)" => ast.Expression(transformExprTree(expr))
        case q"return $expr" => ast.Return(transformExprTree(expr))
        case q"{ ..$stats }" => ast.Block(stats.map(transformStatement))
        case x => throw new MacroException(s"unsupported Scala statement: $x")
      }
    }

    def transformBody(body: Tree): List[CodeAst] = {
      body match {
        case x @ q"{..$values}" => values.map(transformStatement)
        case _ => transformStatement(body) :: Nil
      }
    }

    def transformDef(name: TermName, params: List[List[ValDef]], body: Tree): FunctionDefiniton = {


      FunctionDefiniton(name.encodedName.toString,
          params.flatMap(x => x.map(extractArgName)),
          transformBody(body))
    }

    def extractArgName(arg: ValDef): String = {
      arg match {
        case q"$mods val $name: $tpe = $expr" => name.decodedName.toString
        case q"$mods val $name: $tpe" => name.decodedName.toString
        case x => throw new MacroException(s"unsupported function argument definition $x")
      }
    }

    val structureObjs = content.flatMap {
      case q"$mods def $name[..$tparams](...$paramss): $tpt = $expr" => transformDef(name, paramss, expr) :: Nil
      case x => throw new MacroException(s"unsupported structure: $x")
    }

    val structure = structureObjs.map(x => x.name -> liftStructure(x))

    val newCompanion = companion match {
      case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        val newstats = stats.collect {
          case vx @ q"val asts: $tpe = $expr" =>
            q"""val asts: Map[String, jp.ac.kyotou.kansai.StructureAst]  = ${structure.toMap}"""
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
