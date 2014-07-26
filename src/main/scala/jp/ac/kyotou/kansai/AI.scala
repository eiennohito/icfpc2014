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

  def step(state : (MyList[Int], Int)) : (MyList[Int], Int) = {
    var state_ = state._1
    var dir = state_.cdr.cdr.car
    var cur = state_.car
    var step = state_.cdr.car
    var ndir = next_dir(cur, step, dir)
    var ncur = cur;
    var nstep = step
    if (cur == step) {
      ncur = 0
      nstep = nstep + 1
    } else {
      ncur = cur + 1
    }

    return (MyCons(ncur, MyCons(nstep, MyCons(ndir, MyNil))), ndir)
  }

  def next_dir(cur:Int, step:Int, dir:Int): Int = {
    if (cur == step) {
      return turnRight(dir)
    } 
    return dir
  }

  def entryPoint(world: Int, undoc: Int): (MyList[Int], ((MyList[Int], Int)) => (MyList[Int], Int)) = {
    var init_state = MyCons(0, MyCons(1, MyCons(0, MyNil)))
    return (init_state, step)
  }
}

object AI extends AstCleanup {
  val asts = ???

  def main(args: Array[String]) {
    var gen = NameGen()
	var codeList = CodeGen.emitStructure(cleanAsts.get("entryPoint").get, NameGen())
       codeList ++= CodeGen.emitStructure(cleanAsts.get("step").get, NameGen())
       codeList ++= CodeGen.emitStructure(cleanAsts.get("turnRight").get, NameGen())
       codeList ++= CodeGen.emitStructure(cleanAsts.get("mod").get, NameGen())
       codeList ++= CodeGen.emitStructure(cleanAsts.get("next_dir").get, NameGen())
       codeList ++= List(Label("terminate"))
    // println(codeList.map(CodeGen.show).mkString("", "\n", ""))
    // println("-----")
    println(CodeGen.dereferenceLabels(codeList).map(CodeGen.show).mkString("", "\n", ""))
  }
}
