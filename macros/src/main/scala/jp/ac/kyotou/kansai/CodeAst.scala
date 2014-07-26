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

// GCC instructions
trait Code {
  def show() : String
}
case class Arith(tag: String) extends Code {
  def show(): String = tag
}
case class Ldc(value: Int) extends Code {
  def show(): String = "LDC " + value.toString
}
case class Ld(n: Int, i: Int) extends Code {
  def show(): String = "LD " + n.toString + " " + i.toString
}
case class St(n: Int, i: Int) extends Code {
  def show(): String = "ST " + n.toString + " " + i.toString
}
case class Comp(tag: String) extends Code {
  def show(): String = tag
}
case class Sel(t: Int, f: Int) extends Code {
  def show(): String = "SEL " + t.toString + " " + f.toString
}
case class Join() extends Code {
  def show(): String = "JOIN"
}

// Funcions
case class LoadF(addr : Int) extends Code {
  def show(): String = "LDF " + addr.toString
}
case class App(n : Int) extends Code {
  def show(): String = "AP " + n.toString
}
case class Ret() extends Code {
  def show(): String = "RTN"
}
case class Pop() extends Code {
  def show(): String = "DBUG"
}

// Tail call extensions
case class SelT(t: Int, f: Int) extends Code {
  def show(): String = "TSEL " + t.toString + " " + f.toString
}
case class AppT(n: Int) extends Code {
  def show(): String = "TAP " + n.toString
}

// Lists
case class Cons() extends Code {
  def show(): String = "CONS"
}
case class Car() extends Code {
  def show(): String = "CAR"
}
case class Cdr() extends Code {
  def show(): String = "CDR"
}
