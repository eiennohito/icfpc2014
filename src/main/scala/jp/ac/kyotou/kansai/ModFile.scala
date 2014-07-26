package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-26
 */
@gccCode
class ModFile {
  def mod(a: Int, b: Int): Int = {
    return a - (a / b) * b
  }
}

object ModFile extends AstCleanup {
  val asts = ???

  def main(args: Array[String]) {
    var gen = NameGen()
    println(CodeGen.emitStructure(cleanAsts.get("mod").get, NameGen())
      .map(CodeGen.show).mkString("", "\n", ""))
  }
}
