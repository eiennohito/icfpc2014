package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-26
 */
sealed trait StructureAst {
  def name: String
}
case class FunctionDefiniton(name: String, args: List[String], code: List[StatementAst]) extends StructureAst
case class CaseClassDefinition(name: String, fields: List[FieldInformation]) extends StructureAst

case class FieldInformation(name: String, tpe: String)

sealed trait StatementAst
case class Assign(name: String, result: ExprAst) extends StatementAst
case class Statement(expr: ExprAst) extends StatementAst
case class Return(expr: ExprAst) extends StatementAst
case class Block(content: List[StatementAst]) extends StatementAst
case class WhileStatement(condition: ExprAst, body: List[StatementAst]) extends StatementAst

object CodeStmt {
  def apply(code: Code*) = Statement(LLEmitCode(code.toList))
}

sealed trait ExprAst
case class Literal(value: Int) extends ExprAst

/**
 * Represents a funciton call
 * @param funcName name of function (global or variable)
 * @param args function arguments
 * @param fromVariable if this is true then it was made for variable
 */
case class FunCall(funcName: String, args: List[ExprAst], fromVariable: Boolean = false) extends ExprAst
case class Plus(left: ExprAst, right: ExprAst) extends ExprAst
case class Minus(left: ExprAst, right: ExprAst) extends ExprAst
case class Multiply(left: ExprAst, right: ExprAst) extends ExprAst
case class Divide(left: ExprAst, right: ExprAst) extends ExprAst
case class Reference(name: String, tpe: String = "scala.Int") extends ExprAst
case class ConsAst(left: ExprAst, right: ExprAst) extends ExprAst
case class CarAst(target: ExprAst) extends ExprAst
case class CdrAst(target: ExprAst) extends ExprAst
case class Equals(left: ExprAst, right: ExprAst) extends ExprAst
case class Greater(left: ExprAst, right: ExprAst) extends ExprAst
case class GreaterEquals(left: ExprAst, right: ExprAst) extends ExprAst
case class Lesser(left: ExprAst, right: ExprAst) extends ExprAst
case class LesserEquals(left: ExprAst, right: ExprAst) extends ExprAst
case class NotEquals(left: ExprAst, right: ExprAst) extends ExprAst
case class UnaryNot(expr: ExprAst) extends ExprAst
case class UnaryMinus(expr: ExprAst) extends ExprAst
case class IsAtom(expr: ExprAst) extends ExprAst
case class Debug(expr: ExprAst) extends ExprAst
case class IfExpression(condition: ExprAst, trueBranch: List[StatementAst], falseBranch: List[StatementAst]) extends ExprAst

//Low level expression AST
case class LLLoadAst(frame: Int, pos: Int) extends ExprAst
case class LLStoreAst(frame: Int, pos: Int, expr: ExprAst) extends ExprAst
case class LLAllocateFrameAst(size: Int) extends ExprAst

/**
 * Register function to given name to the current environment frame
 * @param ref
 */
case class LLLoadFunctionAst(name: String) extends ExprAst
case class LLMemberCallAst(func: ExprAst, args: List[ExprAst], call: Int => Code = RApp) extends ExprAst
case class LLEmitCode(code: List[Code], functions: List[String] = Nil) extends ExprAst

//won't appear in the output
case class ApplicationAst(funcName: String, context: ExprAst, args: List[ExprAst], ctxType: String) extends ExprAst
case class ThisRefAst(name: String) extends ExprAst
case class PatternMatchAst(ctx: ExprAst, patterns: List[(CasePatternAst, Option[ExprAst], StatementAst)]) extends ExprAst

sealed trait CasePatternAst
case object WildcardCasePattern extends CasePatternAst
case class LiteralCasePattern(lit: ExprAst) extends CasePatternAst
case class BindingPattern(name: String) extends CasePatternAst
case class ExtractorPattern(tpe: String, pats: List[CasePatternAst]) extends CasePatternAst
case class AltPattern(pats: List[LiteralCasePattern]) extends CasePatternAst


object ForbiddenAsts {
  val forbidden = Set(
    "ApplicationAst",
    "ThisRefAst",
    "PatternMatchAst"
  )

  def check(candadates: TraversableOnce[StructureAst]): List[StructureAst] = {
    candadates.filter {
      case x: FunctionDefiniton =>
        val string = x.toString
        forbidden.forall(y => string.contains(y))
      case _ => true
    }.toList
  }

  def check(map: Map[String, StructureAst]): List[StructureAst] = {
    check(map.view.map(_._2))
  }
}

// GCC instructions
sealed trait Code
case class Arith(tag: String) extends Code
case class Ldc(value: Int) extends Code
case class Ld(n: Int, i: Int) extends Code
case class St(n: Int, i: Int) extends Code
case class Comp(tag: String) extends Code
case class SelA(t: Int, f: Int) extends Code
case class SelL(tl: String, fl: String) extends Code
case class Join() extends Code

// Funcions
case class LoadFA(addr: Int) extends Code
case class LoadFL(label: String) extends Code
case class App(n : Int) extends Code
case class RApp(n: Int) extends Code
case class TRApp(n: Int) extends Code
case class Ret() extends Code
case class Pop() extends Code

// Tail call extensions
case class SelTA(t: Int, f: Int) extends Code
case class SelTL(tl: String, fl: String) extends Code
case class AppT(n: Int) extends Code

// Lists
case class Cons() extends Code
case class Car() extends Code
case class Cdr() extends Code

// Primitive operators
case class Atom() extends Code
case class Dbug() extends Code
case class Dum(s: Int) extends Code

// Label
case class Label(name: String) extends Code
