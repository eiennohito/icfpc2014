package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-26
 */
trait CodeAst
case class Assign(name: String, result: ExprAst) extends CodeAst
case class Expression(expr: ExprAst) extends CodeAst

trait ExprAst
case class Literal(value: Int) extends ExprAst
//ignore this for the time being, will change
case class Application(funcName: String, context: ExprAst, args: List[ExprAst]) extends ExprAst
case class Plus(left: ExprAst, right: ExprAst) extends ExprAst
case class Minus(left: ExprAst, right: ExprAst) extends ExprAst
case class Multiply(left: ExprAst, right: ExprAst) extends ExprAst
case class Divide(left: ExprAst, right: ExprAst) extends ExprAst
case class Reference(name: String) extends ExprAst
