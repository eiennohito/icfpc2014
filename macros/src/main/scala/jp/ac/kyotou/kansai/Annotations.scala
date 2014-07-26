package jp.ac.kyotou.kansai

import scala.language.experimental.macros

import scala.annotation.StaticAnnotation

/**
 * @author eiennohito
 * @since 2014-07-26
 */
class gccCode extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro gccCodeMacroImpl.mainImpl
}

case class MacroException(msg: String) extends RuntimeException(msg)

import scala.reflect.macros.whitebox.Context

class gccCodeMacroImpl(val c: Context) {

  import jp.ac.kyotou.{kansai => ast}
  import c.universe._

  def mainImpl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val trees = annottees.map(_.tree).toList


    if (trees.length != 2) {
      c.error(c.enclosingPosition, "annotated class should have companion object")
    }

    //println(trees)

    val clz :: companion :: Nil = trees

    val content: List[Tree] = clz match {
      case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        stats
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

  def liftStructure(x: StructureAst): Tree = x match {
    case ast.FunctionDefiniton(name, args, code) => q"jp.ac.kyotou.kansai.FunctionDefiniton($name, $args, ${code.map(y => liftCode(y))})"
  }

  def liftCode(x: CodeAst): Tree = x match {
    case ast.Assign(name, result) => q"jp.ac.kyotou.kansai.Assign($name, ${liftExpr(result)})"
    case ast.Expression(expr) => q"jp.ac.kyotou.kansai.Expression(${liftExpr(expr)})"
    case ast.Return(expr) => q"jp.ac.kyotou.kansai.Return(${liftExpr(expr)})"
    case ast.Block(expr) => q"jp.ac.kyotou.kansai.Block(${expr.map(y => liftCode(y))})"
    case ast.IfStatement(cond, tb, fb) =>
      q"jp.ac.kyotou.kansai.IfStatement(${liftExpr(cond)}, ${tb.map(x => liftCode(x))}, ${fb.map(x => liftCode(x))})"
    case ast.WhileStatement(cond, bdy) => q"jp.ac.kyotou.kansai.WhileStatement(${liftExpr(cond)}, ${bdy.map(x => liftCode(x))})"
  }

  def liftExpr (x: ExprAst): Tree = x match {
    case ast.Literal(i) => q"jp.ac.kyotou.kansai.Literal($i)"
    case ast.FunCall(name, args) => q"jp.ac.kyotou.kansai.FunCall($name, ${args.map(y => liftExpr(y))})"
    case ast.Reference(name) => q"jp.ac.kyotou.kansai.Reference($name)"
    case ast.Application(funcName, context, args) => q"jp.ac.kyotou.kansai.Application($funcName, ${liftExpr(context)}, ${args.map(y => liftExpr(y))})"
    case ast.ConsAst(left, right) => q"jp.ac.kyotou.kansai.ConsAst(${liftExpr(left)}, ${liftExpr(right)})"
    case ast.CarAst(target) => q"jp.ac.kyotou.kansai.CarAst(${liftExpr(target)})"
    case ast.CdrAst(target) => q"jp.ac.kyotou.kansai.CdrAst(${liftExpr(target)})"
    case ast.Tuple(constrs) => q"jp.ac.kyotou.kansai.Tuple(${constrs.map(x => liftExpr(x))})"
    case _ => throw new MacroException(s"unsupported expr ast for conversion $x")
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
      case q"$cont.$value" => ast.Application(value.encodedName.toString, transformExprTree(cont), Nil)
      case q"(..$constrs)" => ast.Tuple(constrs.collect {
        case x: Tree => transformExprTree(x)
        case x => throw new MacroException(s"unsupported tree value $x")
      })
      case x => throw new MacroException(s"unsupported expression pattern $x")
    }
  }

  def transformStatement(statement: Tree): CodeAst = {
    //println(s"transform statement: $statement")
    statement match {
      case q"val $nm: $tp = ${value: Int}" => ast.Assign(nm.encodedName.toString, ast.Literal(value))
      case q"val $nm: $tp = $expr" => ast.Assign(nm.encodedName.toString, transformExprTree(expr))
      case q"var $nm: $tp = $expr" => ast.Assign(nm.encodedName.toString, transformExprTree(expr))
      case q"${nm: TermName} = $expr" => ast.Assign(nm.encodedName.toString, transformExprTree(expr))
      case expr @ q"$left.$func(..$args)" => ast.Expression(transformExprTree(expr))
      case expr @ q"$func(..$args)" => ast.Expression(transformExprTree(expr))
      case q"return $expr" => ast.Return(transformExprTree(expr))
      case q"if ($cond) $thenp else $elsep" => ast.IfStatement(
        transformExprTree(cond),
        transformBody(thenp),
        transformBody(elsep)
      )
      case q"while ($cond) $body" => ast.WhileStatement(
        transformExprTree(cond), transformBody(body)
      )
      case q"{}" => ast.Block(Nil)
      case q"{ ..$stats }" => ast.Block(stats.map(transformStatement))
      case x => throw new MacroException(s"unsupported Scala statement: $x")
    }
  }

  def transformBody(body: Tree): List[CodeAst] = {
    body match {
      case q"{}" => Nil
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

}
