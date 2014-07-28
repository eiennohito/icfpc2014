package jp.ac.kyotou.kansai

@gccCode
class sune2AI extends Support {
  /*
    List Utility Code
  */
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

  /*
      MyArray Code
  */

  case class Array2D[T](array: MyArray[MyArray[T]], width: Int,
    get: (Array2D[T], Int, Int) => T,
    put: (Array2D[T], Int, Int, T) => Array2D[T],
    valset: (Array2D[T], Int, T) => Unit,
    from_list: (Array2D[T], MyList[MyList[T]], Int) => Unit
  )

  def Array2D_create[T](width: Int) = {
    val base = MyArray[MyArray[T]]()
    var i = 0
    var arr: MyArray[T] = MyArray[T]()
    while (i < width) {
      base.put(i, arr)
      arr = MyArray[T]()
      i = i + 1
    }
    Array2D[T](base, width, Array2D_get, Array2D_put, Array2D_valset, Array2D_from_list2D)
  }

  def Array2D_get[T](arr: Array2D[T], row: Int, col: Int): T = {
    arr.array.get(col).get(row)
  }
  def Array2D_put[T](arr: Array2D[T], row: Int, col: Int, obj: T): Array2D[T] = {
    arr.array.get(col).put(row, obj)
    return arr
  }

  def Array2D_valset[T](arr: Array2D[T], size: Int, obj: T): Unit = {
    var i = 0
    var j = 0
    var cnt = 0
    while (cnt < size) {
      if (j == arr.width) {
        j = 0
        i = i + 1
      }
      arr.put(arr,i,j,obj)
      j = j + 1
      cnt = cnt + 1
    }
    return
  }

  def Array2D_from_list[T](arr: Array2D[T], lst: MyList[T], y: Int, x: Int) : Unit = {
    if (lst == MyNil) return
    arr.put(arr, y, x, lst.car)
    Array2D_from_list[T](arr, lst.cdr, y, x + 1)
  }

  def Array2D_from_list2D[T](arr: Array2D[T], lst: MyList[MyList[T]], y: Int) : Unit = {
    if (lst == MyNil) return
    Array2D_from_list[T](arr, lst.car, y, 0)
    Array2D_from_list2D[T](arr, lst.cdr, y + 1)
  }


  /*
      Rapid Queue Code
  */

  def rev_aux(l: MyList[Int], r: MyList[Int]): MyList[Int] = {
    if (l == MyNil) return r
    return rev_aux(l.cdr, MyCons(l.car, r))
  }
  def rev(q: MyList[Int]): MyList[Int] = rev_aux(q, MyNil)

  case class MyQueue(f: MyList[Int], b: MyList[Int])

  def empty(): MyQueue = {
    return MyQueue(MyNil, MyNil)
  }
  def isEmpty(q: MyQueue) = q.f == MyNil

  def checkf(q: MyQueue): MyQueue = {
    if (q.f == MyNil) {
      return MyQueue(rev(q.b), MyNil)
    } else {
      return q
    }
  }

  def push(q: MyQueue, x: Int): MyQueue = checkf(MyQueue(q.f, MyCons(x, q.b)))

  def pop(q: MyQueue): (Int, MyQueue) = {
    return (q.f.car, checkf(MyQueue(q.f.cdr, q.b)))
  }
  def head(q: MyQueue): Int = q.f.car
  def tail(q: MyQueue): MyQueue = checkf(MyQueue(q.f.cdr, q.b))

  /*
      sune2AI Code
  */
  case class Point(x : Int, y : Int)
  case class Ghost(vitality : Int, pos : Point, direction : Int)
  case class LambdaMan(vitality : Int, pos : Point, rest : Int)
  case class World(map : Array2D[Int], lambdaMan : LambdaMan, ghosts: MyList[Ghost], height: Int, width: Int)
  case class ArgWorld(map : MyList[MyList[Int]], lambdaMan : LambdaMan, ghosts: MyList[Ghost], rest : Int)

  /*
      Ghost Utility Code
  */
  def thereIsVisibleGhost(ghosts : MyList[Ghost], y : Int, x : Int) : Boolean = {
    if (ghosts == MyNil) {
      return false
    }
    var flag = true
    if (ghosts.car.pos.x != x) {
      flag = false
    }
    if (ghosts.car.pos.y != y) {
      flag = false
    }
    if (flag) {
      if (ghosts.car.vitality != 2) {
        return true
      }
    }
    return thereIsVisibleGhost(ghosts.cdr, y, x)
  }

  def addVisibleGhostToMap(ghosts : MyList[Ghost], map : Array2D[Int]) : Array2D[Int] = {
    if (ghosts == MyNil) return map
    var res = addVisibleGhostToMap(ghosts.cdr, map)
    var ghost = ghosts.car
    if (ghost.vitality != 2) {
      res.put(res, ghost.pos.y, ghost.pos.x, 10)
    }
    return res
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

    // Are gost
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

  /*
      Enty Point
  */
  def entryPoint(ArgWorld: Int, undoc: Int): (Int, (Int, ArgWorld) => (Int, Int)) = {
    return (0,step)
  }

  /*
      Step Function
  */
  def step(state : Int, argWorld : ArgWorld) : (Int, Int) = {
    var height = arraySize2D(argWorld.map)
    var width = arraySize(argWorld.map.car)
    var map = Array2D_create[Int](width)
    map.from_list(map, argWorld.map, 0)
    map = addVisibleGhostToMap(argWorld.ghosts, map)
    var world = World(map, argWorld.lambdaMan, argWorld.ghosts, height, width)

    var pos = world.lambdaMan.pos
    var safeDirection = MyList(1,1,1,1)
    if (world.lambdaMan.vitality <= 300) {
      safeDirection = getSafeDirection(world.ghosts, pos)
    }
    // debug(safeDirection)
    var nextDirection = bfs(world, safeDirection)
    // debug(nextDirection)
    return (0, nextDirection)
  }


  def bfs(world : World, safeDirection : MyList[Int]) : Int = {
    var myPos = world.lambdaMan.pos
    var myVitality = world.lambdaMan.vitality
    var map = world.map
    
    var height = world.height
    var width = world.width

    var queueX = empty()
    var queueY = empty()
    queueX = push(queueX, myPos.x)
    queueY = push(queueY, myPos.y)

    var dist = Array2D_create[Int](width)
    dist.valset(dist, width * height, -1)
    dist.put(dist, myPos.y, myPos.x, 0)

    var prev = Array2D_create[Int](width)
    prev.valset(prev, width * height, -1)
    debug((myPos.y, myPos.x))
    var dy = MyList(-1,0,1,0)
    var dx = MyList(0,1,0,-1)

    var targetY = -1
    var targetX = -1

    var loop = true
    var currentDist = 0
    var d = 0
    var yy = 0
    var xx = 0
    var pred = true
    var content = 0
    var firstLoop = true
    var found = false

    while (loop) {
      var x = head(queueX)
      var y = head(queueY)
      queueX = tail(queueX)
      queueY = tail(queueY)

      if (myVitality == 0) { // standard mode
        if (map.get(map, y, x) == 2) { // pill
          found = true
        }
        if (map.get(map, y, x) == 3) { // power pill
          found = true
        }
      } else { // power pill mode
        if (map.get(map, y, x) == 10) { // visible ghost
          found = true
        }
      }

      if (found) {
        targetY = y
        targetX = x
        loop = false
      } else {
        currentDist = dist.get(dist, y, x)
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
            content = map.get(map, yy, xx)
            pred = true
            if (content == 0) pred = false // wall
            if (myVitality == 0) {
              if (content >= 10) pred = false
            }
            if (pred) { // not wall
              if (dist.get(dist, yy, xx) == -1) {
                dist.put(dist, yy, xx, currentDist + 1)
                queueX = push(queueX, xx)
                queueY = push(queueY, yy)
                prev.put(prev, yy, xx, d)
              }
            }
          }
          d = d + 1
        }
      }
      if (isEmpty(queueX)) {
        loop = false
      }
      firstLoop = false
    }

    // debug(safeDirection)
    // debug(((myPos.x, myPos.y), (targetX, targetY)))
    // debug(world.ghosts)
    if (targetY == -1) return 0
    var lastDirection = -1
    var update = true
    var y = targetY
    var x = targetX
    var direction = 0
    var reverseDirection = 0
    while(update) {
      direction = prev.get(prev, y, x)
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

  /*
    Debug on Scala Utility Code
  */
  def myMain() : Int = {
    var map = MyList(
      MyList(1,1,2,1),
      MyList(1,1,1,0),
      MyList(1,1,1,0),
      MyList(0,2,1,0))
    var lambdaMan = LambdaMan(0, Point(1,1), 0)
    var ghosts = MyList(Ghost(0, Point(1,3), 1))
    var world = ArgWorld(map, lambdaMan, ghosts, 0)
    debug(step(0, world))
    return 0
  }
}

import java.io.PrintWriter

object sune2AI extends AstCleanup(300) {
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
