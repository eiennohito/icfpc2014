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

  def turnRight(dir : Int): Int = {
	return mod(dir + 1, 4)
  }

  def step(state : (Int, Int)) : (Int, Int) = {
    return (1, 1)
  }

  def entryPoint(world: Int, undoc: Int): (Int, ((Int, Int)) => (Int, Int)) = {
    return (42, step)
  }
}

object AI extends AstCleanup {
  val asts = ???

  def main(args: Array[String]) {
    var gen = NameGen()
	var codeList = CodeGen.emitStructure(cleanAsts.get("entryPoint").get, NameGen())
       codeList ++= CodeGen.emitStructure(cleanAsts.get("step").get, NameGen())
    println(codeList.map(CodeGen.show).mkString("", "\n", ""))
    println("-----")
    println(CodeGen.dereferenceLabels(codeList).map(CodeGen.show).mkString("", "\n", ""))
  }
}
