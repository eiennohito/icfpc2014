package jp.ac.kyotou.kansai

trait NameGenerator {
  def get(): String
}
case class NameGen() extends NameGenerator {
  var counter = 0
  def get(): String = {
    counter = counter + 1
    counter.toString
  }
}

object CodeGen {
  def collectLocalVars(code: List[CodeAst], args: List[String]): List[String] = {
    var variables: Set[String] = Set()
    code.foreach(c => c match {
      case Assign(name, _) => variables += name
      case Block(content) => variables ++= collectLocalVars(content, args)
      case _ => ()
    })
    variables.toList.diff(args)
  }

  def emitStructure(st: StructureAst, gen: NameGenerator): List[Code] = {
    st match {
      case FunctionDefiniton(name, args, body) => {
        var res: List[Code] = List(Label("func_" + name))
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
        res ++ body.flatMap(emitCode(_, variables, gen))
      }
      case _ => sys.error("!?")
    }
  }

  def emitCode(code : CodeAst, vars: Map[String, (Int, Int)], gen: NameGenerator): List[Code] = {
    code match {
      case Expression(expr) => emitExpr(expr, vars)
      case Block(content) => content.flatMap(x => emitCode(x, vars, gen))
      case Return(expr) => {
        emitExpr(expr, vars) ++ List(Ret())
      }
      case Assign(name, value) => {
        emitExpr(value, vars) ++ List(St(vars(name)._1, vars(name)._2))
      }
      case IfStatement(cond, t, f) => {
        var res = Label("if" + gen.get()) :: emitExpr(cond, vars)
        var trueL = "true" + gen.get()
        var falseL = "false" + gen.get()
        var afterL = "after" + gen.get()
        res ++= List(SelTL(trueL, falseL))
        res ++= Label(trueL) :: t.flatMap(emitCode(_, vars, gen))
        res ++= List(Ldc(0), Ldc(0), Comp("CEQ"), SelTL(afterL, "terminate"))
        res ++ (Label(falseL) :: f.flatMap(emitCode(_, vars, gen))) ++ List(Label(afterL))
      }
      case WhileStatement(cond, body) => {
        var whileL = "while" + gen.get()
        var bodyL = "while_body" + gen.get()
        var endL = "while_end" + gen.get()
        var res = Label(whileL) :: emitExpr(cond, vars)
        res ++= List(SelTL(bodyL, endL), Label(bodyL)) ++ body.flatMap(emitCode(_, vars, gen))
        res ++ List(Ldc(0), Ldc(0), Comp("CEQ"), SelTL(whileL, "terminate"), Label(endL))
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
      case FunCall(name, args, isLocal) => {
        var res = args.flatMap(emitExpr(_, vars))
        if (isLocal) {
          res ++ List(Ld(vars(name)._1, vars(name)._2), App(args.length))
        } else {
          res ++ List(LoadFL("func_" + name), App(args.length))
        }
      }
      case Plus(l, r) => emitBinaryOp(Arith("ADD"), l, r, vars)
      case Minus(l, r) => emitBinaryOp(Arith("SUB"), l, r, vars)
      case Multiply(l, r) => emitBinaryOp(Arith("MUL"), l, r, vars)
      case Divide(l, r) => emitBinaryOp(Arith("DIV"), l, r, vars)
      case Reference(name, _) => {
        if (vars.get(name) == None) {
          // Reference to function
          List(LoadFL("func_" + name))
        } else {
          // Reference to a local variable
          List(Ld(vars(name)._1, vars(name)._2))
        }
      }
      case ConsAst(l, r) => emitBinaryOp(Cons(), l, r, vars)
      case CarAst(t) => emitExpr(t, vars) ++ List(Car())
      case CdrAst(t) => emitExpr(t, vars) ++ List(Cdr())
      case IsAtom(t) => emitExpr(t, vars) ++ List(Atom())
      case Debug(t) => emitExpr(t, vars) ++ List(Dbug())
      case Equals(l, r) => emitExpr(l, vars) ++ emitExpr(r, vars) ++ List(Comp("CEQ"))
      case Greater(l, r) => emitExpr(l, vars) ++ emitExpr(r, vars) ++ List(Comp("CGT"))
      case GreaterEquals(l, r) => emitExpr(l, vars) ++ emitExpr(r, vars) ++ List(Comp("CGTE"))
      // !!!
      case Lesser(l, r) => emitExpr(Greater(r, l), vars)
      case LesserEquals(l, r) => emitExpr(GreaterEquals(r, l), vars)
      case UnaryNot(exp) => {
        Ldc(1) :: emitExpr(exp, vars) ++ List(Arith("SUB"))
      }
      case Tuple(constrs) => {
        var res = constrs.flatMap(emitExpr(_, vars))
        for (i <- 0 until constrs.length - 1) res ++= List(Cons())
        res
      }
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
      case Atom() => "ATOM"
      case Dbug() => "DBUG"
      case Label(name) => name + ":"
      case _ => "Not implemented yet"
    }
  }

  def dereferenceLabels(code: List[Code]): List[Code] = {
    var lineCount: Int = 0
    var labelMap: Map[String, Int] = Map()
    code.foreach(c => c match {
      case Label(l) => labelMap += (l -> lineCount)
      case _ => lineCount += 1
    })

    code.filter(c => c match {
      case Label(l) => false
      case _ => true
    }).map(c => c match {
      case SelL(tl, fl) => SelA(labelMap(tl), labelMap(fl))
      case LoadFL(l) => LoadFA(labelMap(l))
      case SelTL(tl, fl) => SelTA(labelMap(tl), labelMap(fl))
      case x => x
    })
  }
}
