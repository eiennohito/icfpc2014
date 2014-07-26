package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-26
 */
sealed trait StructureAst
case class FunctionDefiniton(name: String, args: List[String], code: List[CodeAst]) extends StructureAst

sealed trait CodeAst
case class Assign(name: String, result: ExprAst) extends CodeAst
case class Expression(expr: ExprAst) extends CodeAst
case class Return(expr: ExprAst) extends CodeAst

sealed trait ExprAst
case class Literal(value: Int) extends ExprAst
case class FunCall(funcName: String, args: List[ExprAst]) extends ExprAst
case class Plus(left: ExprAst, right: ExprAst) extends ExprAst
case class Minus(left: ExprAst, right: ExprAst) extends ExprAst
case class Multiply(left: ExprAst, right: ExprAst) extends ExprAst
case class Divide(left: ExprAst, right: ExprAst) extends ExprAst
case class Reference(name: String) extends ExprAst

//won't appear in the output
case class Application(funcName: String, context: ExprAst, args: List[ExprAst]) extends ExprAst
