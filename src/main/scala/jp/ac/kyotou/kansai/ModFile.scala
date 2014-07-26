package jp.ac.kyotou.kansai

/**
 * @author eiennohito
 * @since 2014-07-26
 */
@gccCode
class ModFile extends Support {
  def mod(a: Int, b: Int): Int = {
    return a - (a / b) * b
  }

  def max(a: Int, b:Int): Int = {
  	var ans = b
  	if (a > b) {
		ans = a
  	}
  	return ans
  }

  def max2(a: Int, b:Int): Int = {
  	if (a > b) {
  		return a
  	}
  	return b
  }

  def main2(): Int = {
  	return acc(4)
  }

  def acc(n:Int): Int = {
  	var sum = 0
  	var i = 0
  	while (i < 10) {
  		sum = sum + i
  		i = i + 1
  	}
  	return sum
  }

  def acc2(n:Int): Int = {
  	var sum = 0
  	var m = n
  	while (m > 0) {
  		sum = sum + m
  		m = m - 1
  	}
  	return sum
  }

  def turnRight(dir : Int): Int = {
  	var tmp = dir + 1
	return mod(tmp, 4)
  }

  def turnRight2(dir : Int): Int = {
	return mod(dir + 1, 4)

  }

  def next(cur:Int, step:Int, dir:Int): Int = {
  	if (cur == step) {
  		return turnRight(dir)
  	} 
	return dir
  }

  def lstSum(lst:MyList[Int]) : Int = {
  	if (lst.cdr == MyNil) {
  		return 0
  	}
  	return lst.car + lstSum(lst.cdr)
  }
}

object ModFile extends AstCleanup {
  val asts = ???

  def main(args: Array[String]) {
    var gen = NameGen()
	// var codeList = CodeGen.emitStructure(cleanAsts.get("turnRight").get, NameGen())
    //    codeList ++= CodeGen.emitStructure(cleanAsts.get("next").get, NameGen())
    //    codeList ++= CodeGen.emitStructure(cleanAsts.get("mod").get, NameGen())
    //    codeList ++= List(Label("terminate"))
    var codeList = CodeGen.emitStructure(cleanAsts.get("lstSum").get, NameGen())
    println(codeList.map(CodeGen.show).mkString("", "\n", ""))
  }
}
