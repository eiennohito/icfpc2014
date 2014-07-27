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

  def bfs(map : MyList[MyList[Int]], myPos : Point, safeDirection : MyList[Int]) : Int = {
    var height = arraySize2D(map)
    var width = arraySize(map.car)

    var queueSize = height * width + 10
    var queueY = arrayInit(queueSize, 0)
    var queueX = arrayInit(queueSize, 0)
    var qs = 0
    var qt = 0
    queueY = arraySet(queueY, qt, myPos.y)
    queueX = arraySet(queueX, qt, myPos.x)
    qt = qt + 1
    var dist = arrayInit2D(height, width, -1)
    dist = arraySet2D(dist, myPos.y, myPos.x, 0)
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
    var firstLoop = true
    while (loop) {
      var y = arrayGet(queueY, qs)
      var x = arrayGet(queueX, qs)
      
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

          pred = true
          if (yy < 0) pred = false
          if (yy >= height) pred = false
          if (xx < 0) pred = false
          if (xx >= width) pred = false
          if (firstLoop) {
            if (arrayGet(safeDirection, d) == 0) {
              pred = false
            }
          }
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
      firstLoop = false
    }


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

  def getSafeDirection(ghosts : MyList[Ghost], myPos : Point) : MyList[Int] = {
    if (ghosts == MyNil) {
      return MyList(1,1,1,1)
    }

    var res = getSafeDirection(ghosts.cdr, myPos)

    var dy = MyList(-1,0,1,0)
    var dx = MyList(0,1,0,-1)
    var ghost = ghosts.car
    var ghostNextY = ghost.pos.y + arrayGet(dy, ghost.direction)
    var ghostNextX = ghost.pos.x + arrayGet(dx, ghost.direction)
    var d = 0
    var yy = 0
    var xx = 0
    var flag1 = false
    var flag2 = false
    while (d < 4) {
      yy = myPos.y + arrayGet(dy, d)
      xx = myPos.x + arrayGet(dx, d)
      flag1 = false
      flag2 = true
      if (ghost.pos.x != xx) flag2 = false
      if (ghost.pos.y != yy) flag2 = false
      if (flag2) flag1 = true
      flag2 = true
      if (ghostNextX != xx) flag2 = false
      if (ghostNextY != yy) flag2 = false
      if (flag2) flag1 = true
      flag2 = true
      if (ghost.pos.x != xx + arrayGet(dx, d)) flag2 = false
      if (ghost.pos.y != yy + arrayGet(dy, d)) flag2 = false
      if (flag2) flag1 = true

      if (flag1) {
        res = arraySet(res, d, 0)
      }
      d = d + 1
    }
    return res
  }

  case class Point(x : Int, y : Int)
  case class Ghost(vitality : Int, pos : Point, direction : Int)
  case class LambdaMan(vitality : Int, pos : Point, rest : Int)
  case class World(map : MyList[MyList[Int]], lambdaMan : LambdaMan, ghosts: MyList[Ghost], rest : Int)

  def step(state : Int, world : World) : (Int, Int) = {
    var pos = world.lambdaMan.pos
    var safeDirection = getSafeDirection(world.ghosts, pos)
    // debug(safeDirection)
    var nextDirection = bfs(world.map, pos, safeDirection)
    // debug(nextDirection)
    return (0, nextDirection)
  }

  def entryPoint(world: Int, undoc: Int): (Int,  (Int,World) => (Int, Int)) = {
    return (0,step)
  }

  def myMain() : Int = {
    var map = MyList(
      MyList(1,1,2,1),
      MyList(1,1,0,0),
      MyList(1,1,0,0),
      MyList(0,2,1,0))
    var lambdaMan = LambdaMan(0, Point(0,0), 0)
    var ghosts = MyList(Ghost(0, Point(0,1), 0))
    var world = World(map, lambdaMan, ghosts, 0)
    step(0, world)
    return 0
  }  
}

import java.io.PrintWriter

object sune2AI extends AstCleanup {
  val asts = ???

  def main(args: Array[String]) {
    // var tmp = new sune2AI()
    // tmp.myMain()
    // return
	  var codeList = Linker.compileAndLink(cleanAsts, "entryPoint")
    // println(CodeGen.dereferenceLabels(codeList).map(CodeGen.show).mkString("", "\n", ""))
    var p = new PrintWriter("code.txt")
    p.println(CodeGen.dereferenceLabels(codeList).map(CodeGen.show).mkString("", "\n", ""))
    p.close()
  }
}
