package jp.ac.kyotou.kansai

object CodeGen {
  def emitCode(code : CodeAst): List[Code] = {
    code match {
      case Expression(expr) => emitExpr(expr)
      case Block(content) => content.flatMap(x => emitCode(x))
      case _ => sys.error("Not implemented : CodeAst")
    }
  }

  private def emitBinaryOp(op: Code, lhs: ExprAst, rhs: ExprAst): List[Code] = {
    emitExpr(lhs) ++ emitExpr(rhs) ++ List(op)
  }

  def emitExpr(exp : ExprAst): List[Code] = {
    exp match {
      case Literal(v) => List(Ldc(v))
      case Plus(l, r) => emitBinaryOp(Arith("ADD"), l, r)
      case Minus(l, r) => emitBinaryOp(Arith("SUB"), l, r)
      case Multiply(l, r) => emitBinaryOp(Arith("MUL"), l, r)
      case Divide(l, r) => emitBinaryOp(Arith("DIV"), l, r)
      case _ => sys.error("Not implemented : ExprAst")
    }
  }

  def show(code: Code): String = {
    code match {
      case Arith(tag) => tag
      case Ldc(v) => "LDC " + v.toString
      case Ld(n, i) => "LD " + n.toString + " " + i.toString
      case St(n, i) => "ST " + n.toString + " " + i.toString
      case Comp(tag) => tag
      case Sel(t, f) => "SEL " + t.toString + " " + f.toString
      case Join() => "JOIN"
      case LoadF(addr) => "LDF " + addr.toString
      case App(n) => "AP " + n.toString
      case Ret() => "RTN"
      case Pop() => "DBUG"
      case SelT(t, f) => "TSEL " + t.toString + " " + f.toString
      case AppT(n) => "TAP " + n.toString
      case Cons() => "CONS"
      case Car() => "CAR"
      case Cdr() => "CDR"
      case _ => "Not implemented yet"
    }
  }
}
