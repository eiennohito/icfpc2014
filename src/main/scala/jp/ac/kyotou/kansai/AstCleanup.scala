package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-26
 */
abstract class AstCleanup (maxArray: Int = 100) {
  def asts: Map[String, StructureAst]

  lazy val cleanAsts: Map[String, StructureAst] = AstCleanup.cleanupAsts(asts, maxArray)
}

case class RewriteException(msg: String) extends RuntimeException(msg)

class Rewriter(classes: Map[String, CaseClassDefinition]) {

  def rewrite(asts: List[StatementAst]): List[StatementAst] = {
    val withReturns = addReturn(asts)
    rewriteStatements(withReturns)
  }

  val classTypes = classes.keySet

  val tupleElementRe = "_(\\d+)".r

  val tupleName = ".*Tuple(\\d+)".r

  def selectTupleElementLen(ast: ExprAst, num: Int, len: Int) = {
    if (len == num) {
      selectTupleElement(ast, num - 1)
    } else {
      CarAst(selectTupleElement(ast, num - 1))
    }
  }

  def selectTupleElement(ast: ExprAst, i: Int): ExprAst = {
    assert(i >= 0)
    if (i == 0)
      ast
    else CdrAst(selectTupleElement(ast, i - 1))
  }

  def makeTuple(asts: List[ExprAst]): ExprAst = {
    asts match {
      case Nil => throw new RewriteException(s"tuples of size 0 are unsupported")
      case a :: Nil => throw new RewriteException(s"tuples of size 1 are unsupported")
      case a :: b :: Nil => ConsAst(a, b)
      case a :: xs => ConsAst(a, makeTuple(xs))
    }
  }

  val atomTypes = Set (
    "scala.Int",
    "scala.Boolean"
  )

  val listTypes = Set (
    "jp.ac.kyotou.kansai.MyList",
    "jp.ac.kyotou.kansai.MyCons",
    "jp.ac.kyotou.kansai.MyNil"
  )

  val functionName = ".*Function(\\d+)".r

  def makeList(asts: List[ExprAst]): ExprAst = {
    asts match {
      case Nil => throw new RewriteException("empty lists are not allowed")
      case x :: Nil => ConsAst(rewriteExpression(x), Literal(0))
      case x :: xs => ConsAst(rewriteExpression(x), makeList(xs))
    }
  }

  var counter = 0

  def newName = {
    val selected = "___PM_TEMPORARY___" + counter
    counter += 1
    selected
  }

  def rewritePatternMatching(ast: ExprAst, pats: List[(CasePatternAst, Option[ExprAst], StatementAst)]): ExprAst = {

    def combine(leftCheck: Option[ExprAst], guard: Option[ExprAst]) = {
      val newGuard = guard.map(rewriteExpression)
      val newCheck = leftCheck.map(rewriteExpression)

      newCheck match {
        case None => newGuard
        case Some(x) => newGuard match {
          case Some(y) => Some(Multiply(x, y))
          case None => Some(x)
        }
      }
    }

    def rewriteOne(result: ExprAst, pat: CasePatternAst, guard: Option[ExprAst]): (List[(String, ExprAst)], Option[ExprAst]) = {
      pat match {
        case WildcardCasePattern => (Nil, combine(None, guard))
        case LiteralCasePattern(exp) => (Nil, combine(Some(exp), guard))
        case BindingPattern(name) => (name -> result :: Nil, combine(None, guard))
        case ExtractorPattern(tpe, breaks) =>
          def creator(tpe: String): (Int, ExprAst) => ExprAst = {
            tpe match {
              case x if listTypes.contains(x) =>
                (i, e) => CarAst(selectTupleElement(e, i - 1))
              case tupleName(XInt(len)) =>
                (i, e) => selectTupleElementLen(e, i, len)
              case x if classTypes.contains(x) =>
                val ct = classes.get(x).get
                val len = ct.fields.length
                (i, e) => selectTupleElementLen(e, i, len)
              case x => throw new RewriteException(s"unsupported class $x for pattern extraction")
            }
          }
          val fn = creator(tpe)
          val nms = breaks.zipWithIndex.flatMap {
            case (y, i) => rewriteOne(fn(i, result), y, None)._1
          }
          (nms, combine(None, guard))
        case AltPattern(items) => Nil -> Some(items.map(_.lit).reduce[ExprAst]((x, y) => Plus(x, y)))
      }
    }

    def rec(context: ExprAst, rest: List[(CasePatternAst, Option[ExprAst], StatementAst)]): ExprAst = {
      rest match {
        case Nil => Debug(Literal(-124598421))
        case (p, g, e) :: xs =>
          val (assignments, checks) = rewriteOne(context, p, g)
          val body = checks match {
            case Some(x) => List(Statement(IfExpression(x,
              rewriteStatements(e  :: Nil),
              Statement(rec(context, xs)) :: Nil)))
            case None => rewriteStatements(e  :: Nil)
          }

          IfExpression (
          Literal(1),
          assignments.map {
            case (nm, expr) => Assign(nm, expr)
          } ++ body, Nil)
      }
    }

    val name = newName
    val write = Assign(name, rewriteExpression(ast))
    val ctxt = Reference(name)
    IfExpression(
      Literal(0),
      write :: Statement(rec(ctxt, pats)) :: Nil,
      Nil
    )
  }

  def rewriteExpression(expr: ExprAst): ExprAst = {
    expr match {

      case ApplicationAst("isInt", ThisRefAst(_), arg :: Nil, _) =>
        IsAtom(rewriteExpression(arg))

      case ApplicationAst("debug", ThisRefAst(_), arg :: Nil, _) =>
        Debug(rewriteExpression(arg))

      case ApplicationAst(func, ThisRefAst(_), args, ctpe) if !classTypes.contains(ctpe) =>
        FunCall(func, args.map(rewriteExpression))

      case ApplicationAst("at", ctx, Literal(i) :: Nil, "jp.ac.kyotou.kansai.MyList") =>
        CarAst(selectTupleElement(rewriteExpression(ctx), i))

      case ApplicationAst(nm @ ("$eq$eq" | "$bang$eq"), ctx, arg :: Nil, tpe) if listTypes.contains(tpe) =>
        val res = (ctx, arg) match {
          case (Reference("MyNil", _), x) => IsAtom(rewriteExpression(x))
          case (x, Reference("MyNil", _)) => IsAtom(rewriteExpression(x))
          case _ => throw new RewriteException("unsupported binary operation on lists")
        }
        if (nm == "$bang$eq") UnaryNot(res) else res

      case ApplicationAst(name, ctx, arg :: Nil, tpe)  if atomTypes.contains(tpe) =>
        val left = rewriteExpression(ctx)
        val right = rewriteExpression(arg)
        name match {
          case "$plus" => Plus(left, right)
          case "$minus" => Minus(left, right)
          case "$times" => Multiply(left, right)
          case "$div" => Divide(left, right)
          case "$eq$eq" => Equals(left, right)
          case "$greater" => Greater(left, right)
          case "$greater$eq" => GreaterEquals(left, right)
          case "$less" => Lesser(left, right)
          case "$less$eq" => LesserEquals(left, right)
          case "$bang$eq" => UnaryNot(Equals(left, right))
          case "$amp$amp" => Multiply(left, right)
          case "$bar$bar" => Plus(left, right)
          case x => throw new RewriteException(s"unsupported prefixed expression $x")
        }

      case ApplicationAst("apply", ApplicationAst(tupleName(XInt(v)), _, _, _), args, _) =>
        makeTuple(args.map(rewriteExpression))

      case ApplicationAst("apply", Reference("MyCons", _), left :: right :: Nil, _) =>
        ConsAst(rewriteExpression(left), rewriteExpression(right))

      case ApplicationAst("apply", Reference(name, _), args, functionName(XInt(a))) =>
        FunCall(name, args.map(rewriteExpression), fromVariable = true)

      case ApplicationAst("apply", ctx, args, functionName(XInt(a))) =>
        LLMemberCallAst(rewriteExpression(ctx), args.map(rewriteExpression), App)

      case ApplicationAst("apply", Reference("MyList", _), args, _) =>
        if (args.isEmpty) throw new RewriteException("Can't create empty list") else makeList(args)

      case ApplicationAst("apply", Reference("MyArray", _), Nil, "jp.ac.kyotou.kansai.MyArray") =>
        FunCall(AstCleanup.createName, Nil)

      case ApplicationAst("get", ctx, arg :: Nil, "jp.ac.kyotou.kansai.MyArray") =>
        LLMemberCallAst(
          selectTupleElementLen(rewriteExpression(ctx), 1, 2),
          List(rewriteExpression(arg), Literal(0)),
          App)

      case ApplicationAst("put", ctx, args @(arg :: _ :: Nil), "jp.ac.kyotou.kansai.MyArray") =>
        LLMemberCallAst(
          selectTupleElementLen(rewriteExpression(ctx), 2, 2),
          args.map(rewriteExpression),
          App)

      case ApplicationAst("apply", cnt, args, ctpe) if classTypes.contains(ctpe) =>
        makeTuple(args.map(rewriteExpression))

      case ApplicationAst(nm, cnt, Nil, ctpe) if classTypes.contains(ctpe) =>
        val tpdef = classes.get(ctpe).get
        val pos = tpdef.fields.indexWhere(_.name == nm)
        if (pos == -1)
          throw new RewriteException(s"impossible field name: $nm for case class $ctpe")
        else selectTupleElementLen(rewriteExpression(cnt), pos + 1, tpdef.fields.length)

      case ApplicationAst(name, ctx, Nil, ctxtype) =>
        val inner = rewriteExpression(ctx)
        name match {
          case "unary_$bang" => UnaryNot(inner)
          case "unary_$minus" => UnaryMinus(inner)
          case tupleElementRe(XInt(i)) if tupleName.findFirstMatchIn(ctxtype).isDefined =>
            selectTupleElementLen(inner, i, tupleName.findFirstMatchIn(ctxtype).flatMap(m => XInt.unapply(m.group(1))).get)
          case "car" => CarAst(inner)
          case "cdr" => CdrAst(inner)
          case x => throw new RuntimeException("unsupported operation")
        }


      case x: ApplicationAst =>
        throw new RuntimeException(s"invalid application $x")

      case PatternMatchAst(ctx, pats) => rewritePatternMatching(ctx, pats)

      case IfExpression(cond, tb, fb) => IfExpression(rewriteExpression(cond), rewriteStatements(tb), rewriteStatements(fb))
      case FunCall("tupleLast", arg :: Literal(x) :: Nil, _) => CdrAst(selectTupleElement(rewriteExpression(arg), x - 2))
      case FunCall("MyCons", arg1 :: arg2 :: Nil, _) => ConsAst(rewriteExpression(arg1), rewriteExpression(arg2))
      case FunCall(nm, args, _) => FunCall(nm, args.map(rewriteExpression))
      case ConsAst(left, right) => ConsAst(rewriteExpression(left), rewriteExpression(right))
      case Reference("MyNil", _) => Literal(0)
      case x => x
    }
  }

  /**
   * Rewrite asts to have last statement to be returing one
   * @param asts they will be rewritten
   * @return
   */
  def addReturn(asts: List[StatementAst]): List[StatementAst] = {
    val len = asts.length
    val last = asts.last
    val rest = asts.take(len - 1)
    last match {
      case Statement(x) => rest ++ List(Return(x))
      case Block(x) => rest ++ addReturn(x)
      case _ => asts
    }
  }

  def rewriteStatements(code: List[StatementAst]): List[StatementAst] = {
    code.map {
      case Assign(name, result) => Assign(name, rewriteExpression(result))
      case Statement(expr) => Statement(rewriteExpression(expr))
      case Return(expr) => Return(rewriteExpression(expr))
      case Block(expr) => Block(rewriteStatements(expr))
      case WhileStatement(cond, bdy) => WhileStatement(rewriteExpression(cond), rewriteStatements(bdy))
    }
  }
}

object AstCleanup {

  val loadName = "Array____Internal___LOAD"
  val storeName = "Array____Internal___STORE"
  val createName = "Array____Internal___CREATE"
  val initName = "Array_____Internal_____INIT_FIRST_STEP"
  val initName2 = "Array_____Internal_____INIT_SECOND_STEP"


  def generateArraySupport(maxArray: Int): Map[String, FunctionDefiniton] = {
    val create = FunctionDefiniton(createName, Nil, List(
      Statement(LLAllocateFrameAst(maxArray)),
      Statement(LLEmitCode((1 to maxArray).map(x => Ldc(0)).toList)),
      CodeStmt(LoadFL(initName)),
      CodeStmt(RApp(maxArray)),
      CodeStmt(Cons()),
      Return(LLEmitCode(Nil)),
      CodeStmt(Label(initName)),
      Statement(LLLoadFunctionAst(loadName)),
      Statement(LLLoadFunctionAst(storeName)),
      Return(LLEmitCode(Nil))
    ))

    val load = CompilerUtils.emitLookupTable(loadName, List("i", "x"), maxArray, i => LLLoadAst(2, i))
    val store = CompilerUtils.emitLookupTable(storeName, List("i", "x"), maxArray, i => LLStoreAst(2, i, Reference("x")))

    Map(createName -> create, loadName -> load, storeName -> store)
  }

  def cleanupAsts(asts: Map[String, StructureAst], maxArray: Int): Map[String, FunctionDefiniton] = {
    val classes = asts.collect {
      case (k, v: CaseClassDefinition) => k -> v
    }
    val rewriter = new Rewriter(classes)
    val inner = asts.collect {
      case (k, FunctionDefiniton(name, args, code)) =>
        k -> FunctionDefiniton(name, args, rewriter.rewrite(code))
    }.toMap

    val arraySupport = generateArraySupport(maxArray)

    arraySupport ++ inner
  }
}

object XInt {
  def unapply(s: String): Option[Int] = try {
    Some(s.toInt)
  } catch {
    case e: NumberFormatException => None
  }
}
