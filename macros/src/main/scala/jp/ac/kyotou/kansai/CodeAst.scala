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
case class Block(content: List[CodeAst]) extends CodeAst
case class IfStatement(condition: ExprAst, trueBranch: List[CodeAst], falseBranch: List[CodeAst]) extends CodeAst
case class WhileStatement(condition: ExprAst, body: List[CodeAst]) extends CodeAst

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

//won't appear in the output
case class Application(funcName: String, context: ExprAst, args: List[ExprAst], ctxType: String) extends ExprAst
case class Tuple(constructors: List[ExprAst]) extends ExprAst
case class ThisRef(name: String) extends ExprAst

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

// Label
case class Label(name: String) extends Code
