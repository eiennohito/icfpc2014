package jp.ac.kyotou.kansai

object Linker {
  def compileAndLink(asts: Map[String, StructureAst], entryPoint: String): List[Code] = {
    var forbiddendAst = ForbiddenAsts.check(asts)
    if (forbiddendAst != Nil) {
      println(forbiddendAst.mkString("", "\n", ""))
      sys.error("!!! ASTs have forbidden constructors !!!")
    }
    var useFunctions: Set[String] = Set(entryPoint)
    var prev: Set[String] = Set()
    var epAst = asts(entryPoint)
    while (useFunctions != prev) {
      prev = useFunctions
      useFunctions ++= useFunctions.toList.flatMap(func =>
        collectDependencies(asts(func), asts))
    }
    var gen = NameGen()
    var code = CodeGen.emitStructure(epAst, gen)
    useFunctions.toList.foreach(func => code ++= CodeGen.emitStructure(asts(func), gen))
    code ++= List(Label("terminate"))
    return code
  }

  def collectDependencies(func: StructureAst, asts: Map[String, StructureAst]): List[String] = {
    func match {
      case FunctionDefiniton(_, args, body) => {
        body.flatMap(collectDependsStmt(_, asts)).diff(args ++ CodeGen.collectLocalVars(body, args))
      }
    }
  }

  def collectDependsStmt(stmt: StatementAst, asts: Map[String, StructureAst]): List[String] = {
    stmt match {
      case Assign(_, expr) => collectDependsExp(expr, asts)
      case Statement(expr) => collectDependsExp(expr, asts)
      case Return(expr) => collectDependsExp(expr, asts)
      case Block(contents) => contents.flatMap(collectDependsStmt(_, asts))
      case WhileStatement(cond, body) => {
        collectDependsExp(cond, asts) ++ body.flatMap(collectDependsStmt(_, asts))
      }
    }
  }

  def collectDependsExp(expr: ExprAst, asts: Map[String, StructureAst]): List[String] = {
    expr match {
      case Literal(_) => Nil
      case FunCall(name, args, _) => {
        var depends: List[String] = Nil
        if (asts isDefinedAt name) {
          depends = List(name)
        }
        depends ++ args.flatMap(collectDependsExp(_, asts))
      }
      case Plus(l, r) => collectDependsExp(l, asts) ++ collectDependsExp(r, asts)
      case Minus(l, r) => collectDependsExp(l, asts) ++ collectDependsExp(r, asts)
      case Multiply(l, r) => collectDependsExp(l, asts) ++ collectDependsExp(r, asts)
      case Divide(l, r) => collectDependsExp(l, asts) ++ collectDependsExp(r, asts)
      case Reference(name, _) => {
        if (asts isDefinedAt name) {
          List(name)
        } else {
          Nil
        }
      }
      case ConsAst(l, r) => collectDependsExp(l, asts) ++ collectDependsExp(r, asts)
      case CarAst(t) => collectDependsExp(t, asts)
      case CdrAst(t) => collectDependsExp(t, asts)
      case Equals(l, r) => collectDependsExp(l, asts) ++ collectDependsExp(r, asts)
      case Greater(l, r) => collectDependsExp(l, asts) ++ collectDependsExp(r, asts)
      case GreaterEquals(l, r) => collectDependsExp(l, asts) ++ collectDependsExp(r, asts)
      case Lesser(l, r) => collectDependsExp(l, asts) ++ collectDependsExp(r, asts)
      case LesserEquals(l, r) => collectDependsExp(l, asts) ++ collectDependsExp(r, asts)
      case NotEquals(l, r) => collectDependsExp(l, asts) ++ collectDependsExp(r, asts)
      case UnaryNot(t) => collectDependsExp(t, asts)
      case UnaryMinus(t) => collectDependsExp(t, asts)
      case IsAtom(t) => collectDependsExp(t, asts)
      case Debug(t) => collectDependsExp(t, asts)
      case IfExpression(cond, t, f) => {
        var depends = collectDependsExp(cond, asts)
        depends ++ t.flatMap(collectDependsStmt(_, asts)) ++ f.flatMap(collectDependsStmt(_, asts))
      }
    }
  }
}
