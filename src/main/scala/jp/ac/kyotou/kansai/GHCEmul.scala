package jp.ac.kyotou.kansai

import java.io.PrintWriter

/**
 * @author eiennohito
 * @since 2014-07-29
 */
@gccCode
class GHCEmul extends Support {
  case class GHCState(memory: MyArray[Int], registers: MyArray[Int], pc: Int, cnt: Int)
  case class GHCProgram(code: MyArray[GHCInstruction], num: Int)

  case class GHCInstruction(opcode: Int, args: MyList[(Int, Int)])

  case class GHCGhost(program: GHCProgram, state: GHCState)

  def GHC_create() = {
    val memory = MyArray[Int]()
    val registers = MyArray[Int]()
    GHCState(memory, registers, 0, 0)
  }

  def GHC_execute_one(ghost: GHCGhost) = {
    val state = ghost.state
    val prog = ghost.program

    val instr = prog.code.get(state.pc)
    instr.opcode match {
      case 0 => //mov
        val left = instr.args.car
        val right = instr.args.cdr.car
        val whereFrom = right._2
        val src = right._1 match { //no support of reading and writing from and to pc register
          case 0 => state.registers.get(whereFrom)
          case 1 => state.memory.get(state.registers.get(whereFrom))
          case 2 => whereFrom
          case 3 => state.memory.get(whereFrom)
        }
        val whereTo = left._2
        left._1 match {
          case 0 => state.registers.put(whereTo, src)
          case 1 => state.memory.put(state.registers.get(whereTo), src)
          case 3 => state.memory.put(whereTo, src)
        }
    }
    GHCGhost(prog, GHCState(state.memory, state.registers, state.pc + 1, state.cnt + 1))
  }

  def entry(): Unit = {
    val state = GHC_create()
    val prog = MyArray[GHCInstruction]()
    prog.put(0,                 //encoding mov a, 5 as:
      GHCInstruction(0, MyList( // mov
        (0, 0),                 // to the register a
        (2, 5)                  // constant 5
    )))
    val res = GHC_execute_one(GHCGhost(GHCProgram(prog, 0), state))
    debug(res.state.registers.get(0)) //should output 5
  }
}


object GHCEmul extends AstCleanup(1024) {
  val asts = ???

  def main(args: Array[String]) {
    // var tmp = new sune2AI()
    // tmp.myMain()
    // return
    var codeList = Linker.compileAndLink(cleanAsts, "entry")
    // println(CodeGen.dereferenceLabels(codeList).map(CodeGen.show).mkString("", "\n", ""))
    var p = new PrintWriter("code.txt")
    p.println(CodeGen.dereferenceLabels(codeList).map(CodeGen.show).mkString("", "\n", ""))
    p.close()
  }

}
