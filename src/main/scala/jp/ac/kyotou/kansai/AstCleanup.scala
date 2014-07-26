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

  def rewriteExpression(expr: ExprAst): ExprAst = {
    expr match {
      case Application(name, ctx, arg :: Nil) => name match {
        case "$plus" => Plus(rewriteExpression(ctx), rewriteExpression(arg))
        case "$minus" => Minus(rewriteExpression(ctx), rewriteExpression(arg))
        case "$times" => Multiply(rewriteExpression(ctx), rewriteExpression(arg))
        case "$div" => Divide(rewriteExpression(ctx), rewriteExpression(arg))
        case x => throw new RewriteException(s"unsupported prefixed expression $x")
      }
      case x: Application => throw new RuntimeException(s"invalid application $x")
      case x => x
    }
  }

  def rewriteCode(code: List[CodeAst]): List[CodeAst] = {
    code.map {
      case Assign(name, result) => Assign(name, rewriteExpression(result))
      case Expression(expr) => Expression(rewriteExpression(expr))
      case Return(expr) => Return(rewriteExpression(expr))
      case Block(expr) => Block(rewriteCode(expr))
    }
  }
}
