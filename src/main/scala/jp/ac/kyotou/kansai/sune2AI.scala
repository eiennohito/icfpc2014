package jp.ac.kyotou.kansai

@gccCode
class sune2AI extends Support {
  def arrayInit(n : Int, value : Int) : MyList[Int] = {
    if (n == 0) {
      return MyNil
    }
    return MyCons(value, arrayInit(n - 1, value))
  }

  def arrayGet(lst : MyList[Int], index : Int) : Int = {
    if (index == 0) {
      return lst.car
    }
  	return arrayGet(lst.cdr, index - 1)
  }

  def arraySet(lst : MyList[Int], index : Int, value : Int) : MyList[Int] = {
    if (lst == MyNil) {
      return MyNil
    }
    var tmp = lst.car
    if (index == 0) {
      tmp = value
    }
    return MyCons(tmp, arraySet(lst.cdr, index - 1, value))
  }

  def arrayInit2D(n : Int, m : Int, value : Int) : MyList[MyList[Int]] = {
    if (n == 0) {
      return MyNil
    }
    return MyCons(arrayInit(m, value), arrayInit2D(n - 1, m, value))
  }

  def arrayGet2D(lst : MyList[MyList[Int]], y : Int, x : Int) : Int = {
    if (y == 0) {
      return arrayGet(lst.car, x)
    }
    return arrayGet2D(lst.cdr, y-1, x)
  }

  def arraySet2D(lst : MyList[MyList[Int]], y : Int, x : Int, value : Int) : MyList[MyList[Int]] = {
    if (lst == MyNil) {
      return MyNil
    }
    var tmp = lst.car
    if (y == 0) {
      tmp = arraySet(lst.car, x, value)
    }
    return MyCons(tmp, arraySet2D(lst.cdr, y - 1, x, value))
  }

  def arraySize(lst : MyList[Int]) : Int = {
    if (lst == MyNil) {
      return 0
    }
    return 1 + arraySize(lst.cdr)
  }

  def arraySize2D(lst : MyList[MyList[Int]]) : Int = {
    if (lst == MyNil) {
      return 0
    }
    return 1 + arraySize2D(lst.cdr)
  }

  def bfs(map : MyList[MyList[Int]], myY : Int, myX : Int) : Int = {
    var height = arraySize2D(map)
    var width = arraySize(map.car)

    var queueSize = height * width + 10
    var queueY = arrayInit(queueSize, 0)
    var queueX = arrayInit(queueSize, 0)
    var qs = 0
    var qt = 0
    queueY = arraySet(queueY, qt, myY)
    queueX = arraySet(queueX, qt, myX)
    qt = qt + 1
    var dist = arrayInit2D(height, width, -1)
    dist = arraySet2D(dist, myY, myX, 0)
    var prev = arrayInit2D(height, width, -1)

    var dy = MyList(-1,0,1,0)
    var dx = MyList(0,1,0,-1)

    var nearestPillY = -1
    var nearestPillX = -1

    var loop = true
    var currentDist = 0
    var d = 0
    var yy = 0
    var xx = 0
    var pred = true
    var content = 0
    while (loop) {
      var y = arrayGet(queueY, qs)
      var x = arrayGet(queueX, qs)
      debug((qs,y,x,arrayGet2D(dist, y, x)))
      qs = qs + 1

      if (arrayGet2D(map, y, x) == 2) { // pill
        nearestPillY = y
        nearestPillX = x
        loop = false
      } else {
        currentDist = arrayGet2D(dist, y, x)
        d = 0
        while (d < 4) {
          yy = y + arrayGet(dy, d)
          xx = x + arrayGet(dx, d)
          debug(yy,xx)
          pred = true
          if (yy < 0) pred = false
          if (yy >= height) pred = false
          if (xx < 0) pred = false
          if (xx >= width) pred = false
          if (pred) {
            content = arrayGet2D(map, yy, xx)
            if (content != 0) {
              if (arrayGet2D(dist, yy, xx) == -1) {
                dist = arraySet2D(dist, yy, xx, currentDist + 1)
                queueY = arraySet(queueY, qt, yy)
                queueX = arraySet(queueX, qt, xx)
                qt = qt + 1
                prev = arraySet2D(prev, yy, xx, d)
              }
            }
          }
          d = d + 1
        }
      }
      if (qs >= qt) {
        loop = false
      }
    }

    debug(dist)
    debug((nearestPillY, nearestPillX))

    if (nearestPillY == -1) return 0
    var lastDirection = -1
    var update = true
    var y = nearestPillY
    var x = nearestPillX
    var direction = 0
    var reverseDirection = 0
    while(update) {
      direction = arrayGet2D(prev, y, x)
      if (direction == -1) {
        update = false
      } else {
        lastDirection = direction
        reverseDirection = 0
        if (direction >= 2) {
          reverseDirection = direction - 2
        } else {
          reverseDirection = direction + 2
        }
        y = y + arrayGet(dy, reverseDirection)
        x = x + arrayGet(dx, reverseDirection)
      }
    }
    return lastDirection
  }

  def myMain() : Int = {
    var map = MyList(
      MyList(1,1,2,1),
      MyList(2,1,0,0),
      MyList(1,1,0,0),
      MyList(0,2,1,0))
    debug(bfs(map, 1, 1))
    return 0
  }

  def step(state : Int, world : (MyList[MyList[Int]], (Int, (Int, Int), Int), Int)) : (Int, Int) = {
    var map = world._1
    var pos = world._2._2
    var myX = pos._1
    var myY = pos._2
    return (0, bfs(map, myY, myX))
  }

  def entryPoint(world: Int, undoc: Int): (Int,  (Int,(MyList[MyList[Int]], (Int, (Int, Int), Int), Int)) => (Int, Int)) = {
    return (0,step)
  }
}

object sune2AI extends AstCleanup {
  val asts = ???

  def main(args: Array[String]) {
    // var tmp = new sune2AI()
    // tmp.myMain()
    var gen = NameGen()
	  var codeList = CodeGen.emitStructure(cleanAsts.get("entryPoint").get, gen)

    codeList ++= CodeGen.emitStructure(cleanAsts.get("arraySize").get, gen)
    codeList ++= CodeGen.emitStructure(cleanAsts.get("arraySize2D").get, gen)
    codeList ++= CodeGen.emitStructure(cleanAsts.get("arrayInit").get, gen)
    codeList ++= CodeGen.emitStructure(cleanAsts.get("arrayGet").get, gen)
    codeList ++= CodeGen.emitStructure(cleanAsts.get("arraySet").get, gen)
    codeList ++= CodeGen.emitStructure(cleanAsts.get("arrayInit2D").get, gen)
    codeList ++= CodeGen.emitStructure(cleanAsts.get("arrayGet2D").get, gen)
    codeList ++= CodeGen.emitStructure(cleanAsts.get("arraySet2D").get, gen)
    codeList ++= CodeGen.emitStructure(cleanAsts.get("bfs").get, gen)
    codeList ++= CodeGen.emitStructure(cleanAsts.get("step").get, gen)
    
    // codeList ++= CodeGen.emitStructure(cleanAsts.get("arrayInit2D").get, gen)
    codeList ++= List(Label("terminate"))
    // println(codeList.map(CodeGen.show).mkString("", "\n", ""))
    // println("-----")
    println(CodeGen.dereferenceLabels(codeList).map(CodeGen.show).mkString("", "\n", ""))
  }
}
