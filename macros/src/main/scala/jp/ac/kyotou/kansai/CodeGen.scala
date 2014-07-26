package jp.ac.kyotou.kansai

object CodeGen {
  def collectLocalVars(code: List[CodeAst], args: List[String]): List[String] = {
    var variables: Set[String] = Set()
    code.foreach(c => c match {
      case Assign(name, _) => variables += name
      case _ => ()
    })
    variables.toList.diff(args)
  }

  def emitStructure(st: StructureAst): List[Code] = {
    st match {
      case FunctionDefiniton(name, args, body) => {
        var res: List[Code] = List()
        var variables = Map[String, (Int, Int)]()
        for ((arg, i) <- args.zipWithIndex) variables = variables + (arg -> (1, i))
        var localVariables = collectLocalVars(body, args)
        for ((lv, i) <- localVariables.zipWithIndex) variables = variables + (lv -> (0, i))
        // prepare a environment frame for local variables
        for (_ <- 0 until localVariables.length) res = Ldc(0) :: res
        res = LoadFL("body_" + name) :: res
        res = AppT(localVariables.length) :: res
        res = Label("body_" + name) :: res
        res = res.reverse
        body.foreach(code => res = res ++ emitCode(code, variables))
        res
      }
      case _ => sys.error("!?")
    }
  }

  def emitCode(code : CodeAst, vars: Map[String, (Int, Int)]): List[Code] = {
    code match {
      case Expression(expr) => emitExpr(expr, vars)
      case Block(content) => content.flatMap(x => emitCode(x, vars))
      case Assign(name, value) => {
        emitExpr(value, vars) ++ List(St(vars(name)._1, vars(name)._2))
      }
      case _ => sys.error("Not implemented : CodeAst")
    }
  }

  private def emitBinaryOp(op: Code, lhs: ExprAst, rhs: ExprAst,
    vars: Map[String, (Int, Int)]): List[Code] = {
    emitExpr(lhs, vars) ++ emitExpr(rhs, vars) ++ List(op)
  }

  def emitExpr(exp : ExprAst, vars: Map[String, (Int, Int)]): List[Code] = {
    exp match {
      case Literal(v) => List(Ldc(v))
      case Plus(l, r) => emitBinaryOp(Arith("ADD"), l, r, vars)
      case Minus(l, r) => emitBinaryOp(Arith("SUB"), l, r, vars)
      case Multiply(l, r) => emitBinaryOp(Arith("MUL"), l, r, vars)
      case Divide(l, r) => emitBinaryOp(Arith("DIV"), l, r, vars)
      case Reference(name) => List(Ld(vars(name)._1, vars(name)._2))
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
      case SelA(t, f) => "SEL " + t.toString + " " + f.toString
      case SelL(t, f) => "SEL " + t + " " + f
      case Join() => "JOIN"
      case LoadFA(addr) => "LDF " + addr.toString
      case LoadFL(label) => "LDF " + label
      case App(n) => "AP " + n.toString
      case Ret() => "RTN"
      case Pop() => "DBUG"
      case SelTA(t, f) => "TSEL " + t.toString + " " + f.toString
      case SelTL(t, f) => "TSEL " + t + " " + f
      case AppT(n) => "TAP " + n.toString
      case Cons() => "CONS"
      case Car() => "CAR"
      case Cdr() => "CDR"
      case Label(name) => name + ":"
      case _ => "Not implemented yet"
    }
  }
}
