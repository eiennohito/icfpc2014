package jp.ac.kyotou.kansai

@gccCode
class kohamaAI extends Support {
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
    return mod(a * 1664525 + 69081, 10000003)
  }

  def turnRight(dir : Int): Int = {
	return mod(dir + 1, 4)
  }

  def step(state : (Int, Int, Int), world : (Int, (Int, (Int, Int)))) : (((Int, Int, Int)), Int) = {
    var pos = tupleLast(tupleLast(world, 2), 2)
    var nx = pos._1
    var ny = tupleLast(pos, 2)

    var px = state._1
    var py = state._2
    var seed = tupleLast(state, 3)

    if (px == nx) {
      if (py == ny) {
        seed = random(seed)
      }
    }
   var dir = mod(seed, 4)

    return ((nx, ny, seed), dir)
  }

  def entryPoint(world: (Int, (Int, (Int, Int))) , undoc: Int): ((Int, Int, Int),  ((Int, Int, Int), (Int, (Int, (Int, Int)))) => ((Int, Int, Int), Int)) = {
    var pos = tupleLast(tupleLast(world, 2), 2)
    var x = pos._1
    var y = tupleLast(pos, 2)
    var seed = 8
    return ((x, y, seed), step)
  }
}

object kohamaAI extends AstCleanup {
  val asts = ???

  def main(args: Array[String]) {
    var gen = NameGen()
  	var codeList = CodeGen.emitStructure(cleanAsts.get("entryPoint").get, gen)
        codeList ++= CodeGen.emitStructure(cleanAsts.get("step").get, gen)
        codeList ++= CodeGen.emitStructure(cleanAsts.get("mod").get, gen)
        codeList ++= CodeGen.emitStructure(cleanAsts.get("random").get, gen)
        codeList ++= List(Label("terminate"))
    println(codeList.map(CodeGen.show).mkString("", "\n", ""))
    println("-----")
    println(CodeGen.dereferenceLabels(codeList).map(CodeGen.show).mkString("", "\n", ""))
  }
}
