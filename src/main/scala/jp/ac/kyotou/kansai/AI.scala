package jp.ac.kyotou.kansai

@gccCode
class AI extends Support {
  def mod(a: Int, b: Int): Int = {
    return a - (a / b) * b
  }

  def max(a: Int, b:Int): Int = {
    if (a > b) {
      return a
    }
    return b
  }

  def random(a: Int): Int = {
    return mod(a * 1664525 + 107, 1000372)
  }

  def turnRight(dir : Int): Int = {
	return mod(dir + 1, 4)
  }

  @noinline
  def step(state : Int, world : Int) : (Int, Int) = {
    return (random(state), mod(state, 4))
  }

  def entryPoint(world: Int, undoc: Int): (Int, (Int, Int) => (Int, Int)) = {
    return (1023, step)
  }
}

object AI extends AstCleanup {
  val asts = ???

  def main(args: Array[String]) {
    var code = Linker.compileAndLink(cleanAsts, "entryPoint")
    println(code.map(CodeGen.show).mkString("", "\n", ""))
    println("-----")
    println(CodeGen.dereferenceLabels(code).map(CodeGen.show).mkString("", "\n", ""))
  }
}
