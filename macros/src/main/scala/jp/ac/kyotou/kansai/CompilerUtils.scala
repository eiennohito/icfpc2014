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
    FunctionDefiniton(funName, args, Return(rec(0, len)) :: Nil)
  }

  def main(args: Array[String]) {

  }
}


class CodeEmitterTest {

}

object CodeEmitterTest {
  def main(args: Array[String]) {
    val x = Map(
      "debugPrint" -> CompilerUtils.emitLookupTable("debugPrint", "x" :: Nil, 10000, i => Debug(Literal(i))),
       "test" -> FunctionDefiniton("test", Nil, List(
        Return(FunCall("debugPrint", Literal(7) :: Nil, false))
       )))
    val code = Linker.compileAndLink(x, "test")
    println(code.map(CodeGen.show).mkString("", "\n", ""))
    println("-----")
    println(CodeGen.dereferenceLabels(code).map(CodeGen.show).mkString("", "\n", ""))
  }
}
