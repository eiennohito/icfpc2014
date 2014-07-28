package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-27
 */
@gccCode
class ArrayEmitterTest extends Support {
  def test() = {
    val ta = TestA[SomeState](SomeState(8, MyArray[Int]()), getSomething, putSomething)
    ta.upd(ta, 0, 5)
    debug(ta.world(ta, 0))
  }

  def test2(arg: TestA[Int]) = {
    arg.world(arg, 2) + 1
  }

  case class SomeState(a: Int, b: MyArray[Int])
  case class TestA[X](state: X,
                      world: (TestA[X], Int) => Int,
                      upd: (TestA[X], Int, Int) => Unit)

  def getSomething(x: TestA[SomeState], y: Int) = x.state.a + x.state.b.get(y)
  def putSomething(x: TestA[SomeState], where: Int, what: Int) = x.state.b.put(where, what)
}

/**
 * Specify size of arrays as a parameter to AstCleanup
 * Size will be fixed
 */
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
