package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-27
 */
@gccCode
class ArrayEmitterTest extends Support {
  def test() = {
    val x = MyArray[Int]()
    x.put(0, 5)
    debug(x.get(0))
  }
}

object ArrayEmitterTest extends AstCleanup(3) {
  val asts = ???

  def main(args: Array[String]) {

    val astDef = Map("test" -> FunctionDefiniton("test", Nil, List(
      CodeStmt(
        Ldc(5),
        Ldc(6),
        Arith("SUM"),
        Ret()
      )
    )))
    val code = Linker.compileAndLink(cleanAsts, "test")
    println(code.map(CodeGen.show).mkString("", "\n", ""))
    println("-----")
    println(CodeGen.dereferenceLabels(code).map(CodeGen.show).mkString("", "\n", ""))
  }
}
