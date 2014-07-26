package jp.ac.kyotou.kansai

object CodeGen {
  def emitCode(code : CodeAst): List[Code] = {
    code match {
      case Expression(expr) => emitExpr(expr)
      case Block(content) => content.flatMap(x => emitCode(x))
      case _ => sys.error("Not implemented : CodeAst")
    }
  }

  def emitExpr(exp : ExprAst): List[Code] = {
    exp match {
      case Literal(v) => List(Ldc(v))
      case Plus(l, r) => emitExpr(l) ++ emitExpr(r) ++ List(Arith("ADD"))
      case Minus(l, r) => emitExpr(l) ++ emitExpr(r) ++ List(Arith("SUB"))
      case Multiply(l, r) => emitExpr(l) ++ emitExpr(r) ++ List(Arith("MUL"))
      case Divide(l, r) => emitExpr(l) ++ emitExpr(r) ++ List(Arith("DIV"))
      case _ => sys.error("Not implemented : ExprAst")
    }
  }
}
