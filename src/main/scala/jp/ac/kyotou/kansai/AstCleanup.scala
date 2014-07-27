package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-26
 */
trait AstCleanup {
  def asts: Map[String, StructureAst]

  lazy val cleanAsts: Map[String, StructureAst] = AstCleanup.cleanupAsts(asts)
}

case class RewriteException(msg: String) extends RuntimeException(msg)

object AstCleanup {
  def cleanupAsts(asts: Map[String, StructureAst]) = {
    asts.map {
      case (k, v) =>
        k -> (v match {
          case FunctionDefiniton(name, args, code) => FunctionDefiniton(name, args, rewriteCode(code))
        })
    }
  }

  val tupleElementRe = "_(\\d+)".r

  val tupleName = ".*Tuple(\\d+)".r

  def selectTupleElementLen(ast: ExprAst, num: Int, len: Int) = {
    if (len == num) {
      selectTupleElement(ast, num)
    } else {
      CarAst(selectTupleElement(ast, num - 1))
    }
  }

  def selectTupleElement(ast: ExprAst, i: Int): ExprAst = {
    assert(i >= 0)
    if (i == 0)
      ast
    else CdrAst(selectTupleElement(ast, i - 1))
  }

  def makeTuple(asts: List[ExprAst]): ExprAst = {
    asts match {
      case Nil => throw new RewriteException(s"tuples of size 0 are unsupported")
      case a :: Nil => throw new RewriteException(s"tuples of size 1 are unsupported")
      case a :: b :: Nil => ConsAst(a, b)
      case a :: xs => ConsAst(a, makeTuple(xs))
    }
  }

  val allowdedTypes = Set (
    "scala.Int",
    "scala.Boolean",
    "jp.ac.kyotou.kansai.MyList",
    "jp.ac.kyotou.kansai.MyCons",
    "jp.ac.kyotou.kansai.MyNil"
  )

  def rewriteExpression(expr: ExprAst): ExprAst = {
    expr match {

      case Application(func, ThisRef(_), args, _) =>  FunCall(func, args.map(rewriteExpression))

      case Application(name, ctx, arg :: Nil, tpe) =>
        if (!allowdedTypes.contains(tpe)) {
          throw new RewriteException(s"AST rewriter doesn't support binary operations on type $tpe")
        }
        val left = rewriteExpression(ctx)
        val right = rewriteExpression(arg)
        name match {
          case "$plus" => Plus(left, right)
          case "$minus" => Minus(left, right)
          case "$times" => Multiply(left, right)
          case "$div" => Divide(left, right)
          case "$eq$eq" => Equals(left, right)
          case "$greater" => Greater(left, right)
          case "$greater$eq" => GreaterEquals(left, right)
          case "$less" => Lesser(left, right)
          case "$less$eq" => LesserEquals(left, right)
          case x => throw new RewriteException(s"unsupported prefixed expression $x")
        }

      case Application("apply", Application(tupleName(XInt(v)), _, _, _), args, _) =>
        makeTuple(args.map(rewriteExpression))

      case Application("apply", Reference("MyCons", _), left :: right :: Nil, _) =>
        ConsAst(rewriteExpression(left), rewriteExpression(right))

      case Application(name, ctx, Nil, ctxtype) =>
        val inner = rewriteExpression(ctx)
        name match {
          case "unary_$bang" => UnaryNot(inner)
          case "unary_$minus" => UnaryMinus(inner)
          case tupleElementRe(XInt(i)) if tupleName.findFirstMatchIn(ctxtype).isDefined =>
            selectTupleElementLen(inner, i, tupleName.findFirstMatchIn(ctxtype).flatMap(m => XInt.unapply(m.group(1))).get)
          case "car" => CarAst(inner)
          case "cdr" => CdrAst(inner)
          case x => throw new RuntimeException("unsupported operation")
        }


      case x: Application =>
        throw new RuntimeException(s"invalid application $x")

      case FunCall("tupleLast", arg :: Literal(x) :: Nil) => CdrAst(selectTupleElement(rewriteExpression(arg), x - 2))
      case FunCall("MyCons", arg1 :: arg2 :: Nil) => ConsAst(rewriteExpression(arg1), rewriteExpression(arg2))
      case FunCall(nm, args) => FunCall(nm, args.map(rewriteExpression))
      case ConsAst(left, right) => ConsAst(rewriteExpression(left), rewriteExpression(right))
      case Reference("MyNil", _) => Literal(0)
      case x => x
    }
  }

  def rewriteCode(code: List[CodeAst]): List[CodeAst] = {
    code.map {
      case Assign(name, result) => Assign(name, rewriteExpression(result))
      case Expression(expr) => Expression(rewriteExpression(expr))
      case Return(expr) => Return(rewriteExpression(expr))
      case Block(expr) => Block(rewriteCode(expr))
      case IfStatement(cond, tb, fb) => IfStatement(rewriteExpression(cond), rewriteCode(tb), rewriteCode(fb))
      case WhileStatement(cond, bdy) => WhileStatement(rewriteExpression(cond), rewriteCode(bdy))
    }
  }
}

object XInt {
  def unapply(s: String): Option[Int] = try {
    Some(s.toInt)
  } catch {
    case e: NumberFormatException => None
  }
}
