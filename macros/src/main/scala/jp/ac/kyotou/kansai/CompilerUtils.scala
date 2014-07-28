package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-27
 */
object CompilerUtils {
  def emitLookupTable(funName: String, args: List[String], len: Int, ctor: Int => ExprAst): FunctionDefiniton = {
    val arg :: rest = args
    val ref = Reference(arg)
    def rec(left: Int, right: Int): ExprAst = {
      if ((left + 1) == right) ctor(left)
      else {
        val center = left + (right - left) / 2
        IfExpression(
          Lesser(ref, Literal(center)),
          Statement(rec(left, center)) :: Nil,
          Statement(rec(center, right)) :: Nil
        )
      }
    }

    val boundsCheck = IfExpression(
      Greater(ref, Literal(len)),
      List(Statement(Debug(Literal(-100000000))), Statement(Debug(ref)), Return(Literal(-100000000))),
      Return(rec(0, len)) :: Nil
    )

    FunctionDefiniton(funName, args, Statement(boundsCheck) :: Nil)
  }
}
