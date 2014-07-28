package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-27
 */
@gccCode
class ArrayEmitterTest extends Support {
  def test() = {
    val arr2d = Array2D_create[Int](16)
    arr2d.put(arr2d, 9, 12, 5)
    arr2d.put(arr2d, 14, 10, 5)
    arr2d.put(arr2d, 12, 12, 5)
    arr2d.put(arr2d, 6, 12, 5)
    debug(arr2d.get(arr2d, 9, 12) + arr2d.get(arr2d, 14, 10))
  }


  case class Array2D[T](array: MyArray[MyArray[T]], width: Int,
                         get: (Array2D[T], Int, Int) => T,
                         put: (Array2D[T], Int, Int, T) => Unit)

  def Array2D_create[T](width: Int) = {
    val base = MyArray[MyArray[T]]()
    var i = 0
    var arr: MyArray[T] = MyArray[T]()
    while (i < width) {
      base.put(i, arr)
      arr = MyArray[T]()
      i = i + 1
    }
    Array2D[T](base, width, Array2D_get, Array2D_put)
  }

  def Array2D_get[T](arr: Array2D[T], row: Int, col: Int): T = {
    val a = arr.array
    val b = a.get(col)
    b.get(row)
  }
  def Array2D_put[T](arr: Array2D[T], row: Int, col: Int, obj: T): Unit = {
    val a = arr.array
    val b = a.get(col)
    b.put(row, obj)
  }
}

/**
 * Specify size of arrays as a parameter to AstCleanup
 * Size will be fixed
 */
object ArrayEmitterTest extends AstCleanup(16) {
  val asts = ???

  def main(args: Array[String]) {
    val code = Linker.compileAndLink(cleanAsts, "test")
    println(code.map(CodeGen.show).mkString("", "\n", ""))
    println("-----")
    println(CodeGen.dereferenceLabels(code).map(CodeGen.show).mkString("", "\n", ""))
  }
}
