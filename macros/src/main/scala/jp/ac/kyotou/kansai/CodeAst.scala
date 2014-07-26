package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-26
 */
sealed trait StructureAst
case class FunctionDefiniton(name: String, args: List[String], code: List[CodeAst]) extends StructureAst

sealed trait CodeAst {
  def emit(): List[Code]
}
case class Assign(name: String, result: ExprAst) extends CodeAst {
  def emit(): List[Code] = sys.error("Not implemented: Assign")
}
case class Expression(expr: ExprAst) extends CodeAst {
  def emit(): List[Code] = expr.emit()
}
case class Return(expr: ExprAst) extends CodeAst {
  def emit(): List[Code] = sys.error("Not implemented: Return")
}

sealed trait ExprAst {
  def emit(): List[Code]
}
case class Literal(value: Int) extends ExprAst {
  def emit(): List[Code] = List(Ldc(value))
}
case class FunCall(funcName: String, args: List[ExprAst]) extends ExprAst {
  def emit(): List[Code] = sys.error("Not implemented: FucCall")
}
case class Plus(left: ExprAst, right: ExprAst) extends ExprAst {
  def emit(): List[Code] = left.emit() ++ right.emit() ++ List(Arith("ADD"))
}
case class Minus(left: ExprAst, right: ExprAst) extends ExprAst {
  def emit(): List[Code] = left.emit() ++ right.emit() ++ List(Arith("SUB"))
}
case class Multiply(left: ExprAst, right: ExprAst) extends ExprAst {
  def emit(): List[Code] = left.emit() ++ right.emit() ++ List(Arith("MUL"))
}
case class Divide(left: ExprAst, right: ExprAst) extends ExprAst {
  def emit(): List[Code] = left.emit() ++ right.emit() ++ List(Arith("DIV"))
}
case class Reference(name: String) extends ExprAst {
  def emit(): List[Code] = sys.error("Not implemented: Reference")
}

//won't appear in the output
case class Application(funcName: String, context: ExprAst, args: List[ExprAst]) extends ExprAst {
  def emit(): List[Code] = sys.error("Not implemented: Application")
}

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
