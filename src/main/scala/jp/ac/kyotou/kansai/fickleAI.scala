package jp.ac.kyotou.kansai

@gccCode
class fickleAI extends Support {
  def step(state : (Int, Int, Int, Int), world : Int) : (((Int, Int, Int, Int)), Int) = {

    debug(state)

    var up = state._1
    var right = state._2
    var down = state._3
    var left = state._4

    var minv = up
    var mind = 0

    if (minv > right) {
      debug(state)
      minv = right
      mind = 1
    }

    if (minv > down) {
      minv = down
      mind = 2
    }

    if (minv > left) {
      minv = left
      mind = 3
    }

    if (mind == 0) {
      up = up + 1      
    } else {
      if (mind == 1) {
        right = right + 1
      } else {
        if (mind == 2) {
            down = down + 1
        } else {
            left = left + 1
        }
      }
    }

    return (((up, right, down, left)), mind)
  }

  def entryPoint(world: Int, undoc: Int): ((Int, Int, Int, Int), ((Int, Int, Int, Int), Int) => ((Int, Int, Int, Int), Int)) = {
    var up = 0
    var right = 0
    var down = 0
    var left = 0
    return ((up, right, down, left), step)
  }
}

object fickleAI extends AstCleanup {
  val asts = ???

  def main(args: Array[String]) {

    // var ai = new fickleAI
    // println(ai.step((0,0,0,0),0))
    // println(ai.step((1, 2, 0, 3), 0))

    var gen = NameGen()
  	var codeList = CodeGen.emitStructure(cleanAsts.get("entryPoint").get, gen)
        codeList ++= CodeGen.emitStructure(cleanAsts.get("step").get, gen)
        codeList ++= List(Label("terminate"))
    println(codeList.map(CodeGen.show).mkString("", "\n", ""))
    println("-----")
    println(CodeGen.dereferenceLabels(codeList).map(CodeGen.show).mkString("", "\n", ""))
  }
}
