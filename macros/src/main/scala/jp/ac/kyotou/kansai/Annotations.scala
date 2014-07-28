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

    val checked = c.typecheck(clz)

    val content: List[Tree] = checked match {
      case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        stats
    }

    val structureObjs = content.flatMap {
      case q"$mods def $name[..$tparams](...$paramss): $tpt = $expr" => transformDef(name, paramss, expr) :: Nil
      case tree @ q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }"
        if mods.hasFlag(Flag.CASE) =>  transformClass(tree, tpname, paramss.flatten) :: Nil
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

  implicit val liftableStructure = Liftable[StructureAst] { s => liftStructure(s) }
  implicit val liftableCode = Liftable[StatementAst] { s => liftCode(s) }
  implicit val lifatbleExpr = Liftable[ExprAst] { s => liftExpr(s) }

  implicit val liftableTE = Liftable[FieldInformation] { s => s match {
      case FieldInformation(nm, tpe) => q"jp.ac.kyotou.kansai.FieldInformation($nm, $tpe)"
    }
  }

  implicit val litePattern = Liftable[LiteralCasePattern] { s => liftCasePattern(s) }

  implicit val liftableCasePattern = Liftable[CasePatternAst] { s => liftCasePattern(s) }

  def liftCasePattern(x: CasePatternAst): Tree = x match {
    case WildcardCasePattern => q"jp.ac.kyotou.kansai.WildcardCasePattern"
    case LiteralCasePattern(lit) => q"jp.ac.kyotou.kansai.LiteralCasePattern($lit)"
    case BindingPattern(nm) => q"jp.ac.kyotou.kansai.BindingPattern($nm)"
    case ExtractorPattern(tpe, pats) => q"jp.ac.kyotou.kansai.ExtractorPattern($tpe, $pats)"
    case AltPattern(pats) => q"jp.ac.kyotou.kansai.AltPattern($pats)"
  }

  def liftStructure(x: StructureAst): Tree = x match {
    case ast.FunctionDefiniton(name, args, code) => q"jp.ac.kyotou.kansai.FunctionDefiniton($name, $args, $code)"
    case ast.CaseClassDefinition(name, args) => q"jp.ac.kyotou.kansai.CaseClassDefinition($name, $args)"
  }

  def liftCode(x: StatementAst): Tree = x match {
    case ast.Assign(name, result) => q"jp.ac.kyotou.kansai.Assign($name, $result)"
    case ast.Statement(expr) => q"jp.ac.kyotou.kansai.Statement($expr)"
    case ast.Return(expr) => q"jp.ac.kyotou.kansai.Return(${liftExpr(expr)})"
    case ast.Block(expr) => q"jp.ac.kyotou.kansai.Block(${expr.map(y => liftCode(y))})"
    case ast.WhileStatement(cond, bdy) => q"jp.ac.kyotou.kansai.WhileStatement(${liftExpr(cond)}, ${bdy.map(x => liftCode(x))})"
  }

  def liftExpr (x: ExprAst): Tree = x match {
    case ast.Literal(i) => q"jp.ac.kyotou.kansai.Literal($i)"
    case ast.FunCall(name, args, tx) => q"jp.ac.kyotou.kansai.FunCall($name, $args, $tx)"
    case ast.Reference(name, tpe) => q"jp.ac.kyotou.kansai.Reference($name, $tpe)"
    case ast.ApplicationAst(funcName, context, args, tpe) => q"jp.ac.kyotou.kansai.ApplicationAst($funcName, $context, $args, $tpe)"
    case ast.ConsAst(left, right) => q"jp.ac.kyotou.kansai.ConsAst(${liftExpr(left)}, ${liftExpr(right)})"
    case ast.CarAst(target) => q"jp.ac.kyotou.kansai.CarAst(${liftExpr(target)})"
    case ast.CdrAst(target) => q"jp.ac.kyotou.kansai.CdrAst(${liftExpr(target)})"
    case ast.ThisRefAst(name) => q"jp.ac.kyotou.kansai.ThisRefAst($name)"
    case ast.IfExpression(cond, tb, fb) =>
      q"jp.ac.kyotou.kansai.IfExpression($cond, $tb, $fb)"
    case PatternMatchAst(ctx, pats) => q"jp.ac.kyotou.kansai.PatternMatchAst($ctx, $pats)"
    case _ => throw new MacroException(s"unsupported expr ast for conversion $x")
  }

  def checkArgs(inargs: List[ValDef], outargs: List[Tree]): Boolean = {
    if (inargs.length != outargs.length) false
    else {
      inargs.zip(outargs).forall {
        case (
          q"$mods val $nm1: $tpe = $bdy",
          q"${nm2: TermName}"
          ) =>
          nm1 == nm2
        case _ => false
      }
    }
  }

  def transformExprTree(tree: Tree): ExprAst = {
    tree match {
      case q"$left.$func[..$tpe](..$args)" => ast.ApplicationAst(func.encodedName.toString,
        transformExprTree(left),
        args.collect {
          case x: Tree => transformExprTree(x)
          case x => throw new MacroException(s"unsupported tree value $x")
        },
        left.tpe.typeSymbol.fullName
      )
      case q"$x.this" => ast.ThisRefAst(x.encodedName.toString)
      case q"${lit: Int}" => ast.Literal(lit)
      case q"${lit: Boolean}" => ast.Literal(if (lit) 1 else 0)
      case q"${ref: TermName}" => ast.Reference(ref.decodedName.toString, tree.tpe.typeSymbol.fullName)
      case q"$cont.$value" => ast.ApplicationAst(value.encodedName.toString, transformExprTree(cont), Nil, cont.tpe.typeSymbol.fullName)
      case q"{ (..$inargs) => $left.$call(..$outargs) }" if checkArgs(inargs, outargs) =>
        ast.Reference(call.encodedName.toString, tree.tpe.typeSymbol.fullName)
      case q"if ($cond) $thenp else $elsep" => ast.IfExpression(
        transformExprTree(cond),
        transformBody(thenp),
        transformBody(elsep)
      )
      case q"$expr match { case ..$pats }" =>
        PatternMatchAst(transformExprTree(expr), transformPatterns(pats))
      case x => throw new MacroException(s"unsupported expression pattern $x")
    }
  }

  def transformPatterns(trees: List[Tree]) = {
    trees.map(transformPattern)
  }

  def transformPattern(tree: Tree): (CasePatternAst, Option[ExprAst], ExprAst) = {
    tree match {
      case cq"$pat => $bdy" => (transformPatternBody(pat), None, transformStatement(bdy))
      case cq"$pat if $cnd => $bdy" => (transformPatternBody(pat), Some(transformExprTree(cnd)), transformStatement(bdy))
    }
  }

  def transformAltPattern(tree: Tree): LiteralCasePattern = {
    val tfLast = transformPatternBody(tree)
    tfLast match {
      case p: LiteralCasePattern => p
      case x => throw new MacroException("alt patterns support only literal patterns")
    }
  }

  def transformPatternBody(tree: Tree): CasePatternAst = {
    tree match {
      case pq"_" => WildcardCasePattern
      case pq"$x" => LiteralCasePattern(transformExprTree(x))
      case pq"$name @ $bind" => BindingPattern(name.encodedName.toString)
      case pq"$tpe(..$pats)" => ExtractorPattern(tree.tpe.typeSymbol.fullName, pats.map(transformPatternBody))
      case pq"$pat | ..$every" => AltPattern(transformAltPattern(pat) :: every.map(transformAltPattern))
    }
  }

  def transformStatement(statement: Tree): StatementAst = {
    //println(s"transform statement: $statement")
    statement match {
      case q"$mods val $nm: $tp = ${value: Int}" => ast.Assign(nm.encodedName.toString, ast.Literal(value))
      case q"$mods val $nm: $tp = $expr" => ast.Assign(nm.encodedName.toString, transformExprTree(expr))
      case q"$mods var $nm: $tp = $expr" => ast.Assign(nm.encodedName.toString, transformExprTree(expr))
      case q"${nm: TermName} = $expr" => ast.Assign(nm.encodedName.toString, transformExprTree(expr))
      case expr @ q"$left.$func[..$tpe](..$args)" => ast.Statement(transformExprTree(expr))
      case expr @ q"$func[..$tpe](..$args)" => ast.Statement(transformExprTree(expr))
      case expr @ q"$left.$right" => ast.Statement(transformExprTree(expr))
      case q"return $expr" => ast.Return(transformExprTree(expr))
      case q"if ($cond) $thenp else $elsep" => ast.Statement(ast.IfExpression(
        transformExprTree(cond),
        transformBody(thenp),
        transformBody(elsep)
      ))
      case q"while ($cond) $body" => ast.WhileStatement(
        transformExprTree(cond), transformBody(body)
      )
      case x => throw new MacroException(s"unsupported Scala statement: $x")
    }
  }

  def transformBody(body: Tree): List[StatementAst] = {
    body match {
      case q"{}" => Nil
      case x @ q"{..$values}" => values.map(transformStatement)
      case _ => transformStatement(body) :: Nil
    }
  }

  def transformClass(tree: Tree, name: TypeName, params: List[ValDef]): CaseClassDefinition = {
    val parAst = params.map {
      case q"$mods val $nm: $tpe = $expr" => FieldInformation(nm.encodedName.toString, tpe.tpe.typeSymbol.fullName)
      case x => throw new MacroException(s"unsupported field for case class $x")
    }

    CaseClassDefinition(tree.symbol.fullName, parAst)
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
