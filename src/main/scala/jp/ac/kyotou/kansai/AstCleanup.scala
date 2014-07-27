package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-26
 */
trait AstCleanup {
  def asts: Map[String, StructureAst]

  lazy val cleanAsts: Map[String, StructureAst] = AstCleanup.cleanupAsts(asts)
}

case class RewriteException(msg: String) extends RuntimeException(msg)

object AstCleanup {
  def cleanupAsts(asts: Map[String, StructureAst]) = {
    asts.map {
      case (k, v) =>
        k -> (v match {
          case FunctionDefiniton(name, args, code) =>
            val withReturns = addReturn(code)
            FunctionDefiniton(name, args, rewriteStatements(withReturns))
        })
    }
  }

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

  def rewriteExpression(expr: ExprAst): ExprAst = {
    expr match {

      case Application("isInt", ThisRef(_), arg :: Nil, _) =>
        IsAtom(rewriteExpression(arg))

      case Application("debug", ThisRef(_), arg :: Nil, _) =>
        Debug(rewriteExpression(arg))

      case Application(func, ThisRef(_), args, _) =>  FunCall(func, args.map(rewriteExpression))

      case Application("at", ctx, Literal(i) :: Nil, "jp.ac.kyotou.kansai.MyList") =>
        CarAst(selectTupleElement(rewriteExpression(ctx), i))

      case Application(nm @ ("$eq$eq" | "$bang$eq"), ctx, arg :: Nil, tpe) if listTypes.contains(tpe) =>
        val res = (ctx, arg) match {
          case (Reference("MyNil", _), x) => IsAtom(rewriteExpression(x))
          case (x, Reference("MyNil", _)) => IsAtom(rewriteExpression(x))
          case _ => throw new RewriteException("unsupported binary operation on lists")
        }
        if (nm == "$bang$eq") UnaryNot(res) else res

      case Application(name, ctx, arg :: Nil, tpe)  if atomTypes.contains(tpe) =>
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
          case x => throw new RewriteException(s"unsupported prefixed expression $x")
        }

      case Application("apply", Application(tupleName(XInt(v)), _, _, _), args, _) =>
        makeTuple(args.map(rewriteExpression))

      case Application("apply", Reference("MyCons", _), left :: right :: Nil, _) =>
        ConsAst(rewriteExpression(left), rewriteExpression(right))

      case Application("apply", Reference(name, _), args, functionName(XInt(a))) =>
        FunCall(name, args.map(rewriteExpression), fromVariable = true)

      case Application("apply", Reference("MyList", _), args, _) =>
        if (args.isEmpty) throw new RewriteException("Can't create empty list") else makeList(args)

      case Application(name, ctx, Nil, ctxtype) =>
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


      case x: Application =>
        throw new RuntimeException(s"invalid application $x")

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

object XInt {
  def unapply(s: String): Option[Int] = try {
    Some(s.toInt)
  } catch {
    case e: NumberFormatException => None
  }
}
