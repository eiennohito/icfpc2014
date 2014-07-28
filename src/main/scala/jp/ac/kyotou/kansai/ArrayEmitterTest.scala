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

    val arr2d = Array2D_create[Int](4)
    arr2d.put(arr2d, 1, 1, 5)
    debug(arr2d.get(arr2d, 1, 1))
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


  case class Array2D[T](array: MyArray[T], width: Int,
                         get: (Array2D[T], Int, Int) => T,
                         put: (Array2D[T], Int, Int, T) => Unit)

  def Array2D_create[T](width: Int) = {
    val internal = MyArray[T]()
    Array2D[T](internal, width, Array2D_get, Array2D_put)
  }

  def Array2D_get[T](arr: Array2D[T], row: Int, col: Int): T = arr.array.get(row * arr.width + col)
  def Array2D_put[T](arr: Array2D[T], row: Int, col: Int, obj: T): Unit = {
    arr.array.put(row * arr.width + col, obj)
  }
}

/**
 * Specify size of arrays as a parameter to AstCleanup
 * Size will be fixed
 */
object ArrayEmitterTest extends AstCleanup(16) {
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
