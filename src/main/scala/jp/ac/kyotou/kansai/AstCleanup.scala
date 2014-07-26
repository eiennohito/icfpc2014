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

  val tupleName = "Tuple(\\d+)".r

  def selectTupleElement(ast: ExprAst, i: Int): ExprAst = {
    if (i == 0)
      ast
    else CdrAst(selectTupleElement(ast, i - 1))
  }

  def makeTuple(asts: List[ExprAst]): ExprAst = {
    asts match {
      case a :: Nil => throw new RewriteException(s"tuples of size 1 are unsupported")
      case a :: b :: Nil => ConsAst(a, b)
      case a :: xs => ConsAst(a, makeTuple(xs))
    }
  }

  def rewriteExpression(expr: ExprAst): ExprAst = {
    expr match {
      case Application(name, ctx, arg :: Nil) =>

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

      case Application(tupleName(XInt(v)), Reference("scala"), args) =>
        makeTuple(args.map(rewriteExpression))

      case Application(name, ctx, Nil) =>
        val inner = rewriteExpression(ctx)
        name match {
          case "unary_$bang" => UnaryNot(inner)
          case "unary_$minus" => UnaryMinus(inner)
          case tupleElementRe(XInt(i)) => CarAst(selectTupleElement(inner, i - 1))
          case "car" => CarAst(inner)
          case "cdr" => CdrAst(inner)
          case x => throw new RuntimeException("unsupported operation")
        }
      case x: Application => throw new RuntimeException(s"invalid application $x")
      case FunCall("tupleLast", arg :: Literal(x) :: Nil) => CdrAst(selectTupleElement(rewriteExpression(arg), x - 2))
      case FunCall("MyCons", arg1 :: arg2 :: Nil) => ConsAst(rewriteExpression(arg1), rewriteExpression(arg2))
      case FunCall(nm, args) => FunCall(nm, args.map(rewriteExpression))
      case ConsAst(left, right) => ConsAst(rewriteExpression(left), rewriteExpression(right))
      case Reference("MyNil") => Literal(0)
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
