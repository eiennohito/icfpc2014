[package jp.ac.kyotou.kansai

@gccCode
class sune2AIRAL extends Support {
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

  case class Array2D[T](array: MyArray[T], width: Int,
    get: (Array2D[T], Int, Int) => T,
    put: (Array2D[T], Int, Int, T) => Unit,
    valset: (Array2D[T], Int, T) => Unit,
    from_list: (Array2D[T], MyList[MyList[T]], Int) => Unit
  )

  def Array2D_create[T](width: Int) = {
    val internal = MyArray[T]()
    Array2D[T](internal, width, Array2D_get, Array2D_put, Array2D_valset, Array2D_from_list2D)
  }

  def Array2D_get[T](arr: Array2D[T], row: Int, col: Int): T = arr.array.get(row * arr.width + col)
  def Array2D_put[T](arr: Array2D[T], row: Int, col: Int, obj: T): Unit = {
    arr.array.put(row * arr.width + col, obj)
  }

  def Array2D_valset[T](arr: Array2D[T], size: Int, obj: T): Unit = {
    var i = 0
    while (i < size) {
      arr.array.put(i,obj)
      i = i + 1
    }
    return
  }

  def Array2D_from_list[T](arr: Array2D[T], lst: MyList[T], cnt: Int) : Int = {
    if (lst == MyNil) return cnt
    arr.array.put(cnt, lst.car)
    return Array2D_from_list[T](arr, lst.cdr, cnt + 1)
  }

  def Array2D_from_list2D[T](arr: Array2D[T], lst: MyList[MyList[T]], cnt: Int) : Unit = {
    if (lst == MyNil) return
    var cnt2 = Array2D_from_list[T](arr, lst.car, cnt)
    Array2D_from_list2D[T](arr, lst.cdr, cnt2)
  }

  /*
   RAL Code
   */

  // This is an implementation of BinaryRandomAccessList
  /*
   cons_RAL, head_RAL, tail_RAL: O(log n)
   lookup_RAL, update_RAL, size_RAL: O(log n)
   */

  case class Tree[T](w: Int, v: MyList[T], ch: MyList[Tree[T]])
  def leaf_RAL[T](t: T) = Tree[T](1, MyList(t), MyNil)
  def emptyLeaf_RAL[T]() = Tree[T](1, MyNil, MyNil)
  def isLeaf_RAL[T](v: Tree[T]) = v.ch == MyNil
  def node_RAL[T](v: T, l: Tree[T], r: Tree[T]) = {
    Tree(sizeTree_RAL(l) + sizeTree_RAL(r), MyList(v), MyList(l, r))
  }
  def leftChild_RAL[T](t: Tree[T]) = t.ch.car
  def rightChild_RAL[T](t: Tree[T]) = t.ch.cdr.car

  case class Digit[T](d: Int, t: Tree[T])
  // type RList = MyList[Digit]
  def isEmpty_RAL[T](l: MyList[Digit[T]]) = l == MyNil
  def zero_RAL[T](): Digit[T] = Digit(0, emptyLeaf_RAL())

  def sizeTree_RAL[T](t: Tree[T]): Int = {
    if (isLeaf_RAL(t)) {
      return 1
    } else {
      return t.w
    }
  }
  def link_RAL[T](t1: Tree[T], t2: Tree[T]) = Tree(sizeTree_RAL(t1) + sizeTree_RAL(t2), MyNil, MyList(t1, t2))
  def consTree_RAL[T](t: Tree[T], l: MyList[Digit[T]]): MyList[Digit[T]] = {
    if (isEmpty_RAL(l)) {
      return MyList(Digit(1, t))
    } else {
      if (l.car.d == 0) {
        return MyCons(Digit(1, t), l.cdr)
      } else {
        return MyCons(zero_RAL(), consTree_RAL(link_RAL(t, l.car.t), l.cdr))
      }
    }
  }

  def unconsTreeAux_RAL[T](t: MyList[Digit[T]]): (Tree[T], MyList[Digit[T]]) = {
    var ts = unconsTree_RAL(t.cdr)
    return (leftChild_RAL(ts._1), MyCons(Digit(1, rightChild_RAL[T](ts._1)), ts._2))
  }

  def unconsTree_RAL[T](t: MyList[Digit[T]]): (Tree[T], MyList[Digit[T]]) = {
    var td: Digit[T] = t.car
    if (td.d == 1) {
      if (t.cdr == MyNil) {
        // [One t]
        return (td.t, MyNil)
      } else {
        // [One t, ... ]
        return (td.t, MyCons(zero_RAL[T](), t.cdr))
      }
    } else {
      // [Zero, ... ]
      return unconsTreeAux_RAL(t)
    }
  }

  def cons_RAL[T](x: T, ts: MyList[Digit[T]]) = consTree_RAL(leaf_RAL(x), ts)
  def head_RAL[T](ts: MyList[Digit[T]]) = unconsTree_RAL(ts)._1.v
  def tail_RAL[T](ts: MyList[Digit[T]]) = unconsTree_RAL(ts)._2
  def size_RAL[T](ts: MyList[Digit[T]]): Int = {
    if (isEmpty_RAL(ts)) return 0
    if (ts.car.d == 0) {
      return size_RAL(ts.cdr)
    } else {
      return sizeTree_RAL(ts.car.t) + size_RAL(ts.cdr)
    }
  }

  def lookupTree_RAL[T](i: Int, t: Tree[T]): T = {
    if (isLeaf_RAL(t)) {
      return t.v.car
    } else {
      if (i < (t.w / 2)) {
        return lookupTree_RAL(i, leftChild_RAL(t))
      } else {
        return lookupTree_RAL(i - (t.w / 2), rightChild_RAL(t))
      }
    }
  }
  def updateTree_RAL[T](i: Int, v: T, t: Tree[T]): Tree[T] = {
    if (isLeaf_RAL(t)) {
      if (i == 0) {
        return leaf_RAL(v)
      } else {
        return emptyLeaf_RAL()
      }
    } else {
      if (i < (t.w / 2)) {
        return Tree(t.w, MyNil, MyList(updateTree_RAL(i, v, leftChild_RAL(t)), rightChild_RAL(t)))
      } else {
        return Tree(t.w, MyNil, MyList(leftChild_RAL(t), updateTree_RAL(i - (t.w / 2), v, rightChild_RAL(t))))
      }
    }
  }


  // ral[i]
  def lookup_RAL[T](i: Int, ral: MyList[Digit[T]]): T = {
    if (ral.car.d == 0) {
      return lookup_RAL(i, ral.cdr)
    } else {
      if (i < sizeTree_RAL(ral.car.t)) {
        return lookupTree_RAL(i, ral.car.t)
      } else {
        return lookup_RAL(i - sizeTree_RAL(ral.car.t), ral.cdr)
      }
    }
  }

  // ral[i] = v
  def update_RAL[T](i: Int, v: T, ral: MyList[Digit[T]]): MyList[Digit[T]] = {
    if (ral.car.d == 0) {
      return MyCons(zero_RAL(), update_RAL(i, v, ral.cdr))
    } else {
      if (i < sizeTree_RAL(ral.car.t)) {
        return MyCons(Digit(1, updateTree_RAL[T](i, v, ral.car.t)), ral.cdr)
      } else {
        return MyCons(Digit(1, ral.car.t), update_RAL(i - sizeTree_RAL(ral.car.t), v, ral.cdr))
      }
    }
  }

  def fromList_RAL[T](l: MyList[T]): MyList[Digit[T]] = {
    if (l == MyNil) {
      return MyNil
    } else {
      return cons_RAL(l.car, fromList_RAL(l.cdr))
    }
  }

  // MyList[MyList[T]] -> MyList[Digit[MyList[Digit[T]]]] (2D random access list)
  def fromList2D_RAL[T](l: MyList[MyList[T]]): MyList[Digit[MyList[Digit[T]]]] = {
    if (l == MyNil) {
      return MyNil
    } else {
      return cons_RAL(fromList_RAL(l.car), fromList2D_RAL(l.cdr))
    }
  }
  // return l[y][x]
  def lookup2D_RAL[T](x: Int, y: Int, l: MyList[Digit[MyList[Digit[T]]]]) = lookup_RAL(y, lookup_RAL(x, l))
  // l[y][x] = v
  def update2D_RAL[T](x: Int, y: Int, v: T, l: MyList[Digit[MyList[Digit[T]]]]) = {
    update_RAL(y, update_RAL(x, v, lookup_RAL(y, l)), l)
  }

  // create array filled with a constant
  def constant_RAL[T](size: Int, v: T): MyList[Digit[T]] = {
    if (size == 0) {
      return MyNil
    } else {
      return cons_RAL(v, constant_RAL(size - 1, v))
    }
  }
  def constant2D_RAL[T](h: Int, w: Int, v: T): MyList[Digit[MyList[Digit[T]]]] = {
    if (h == 0) {
      return MyNil
    } else {
      return cons_RAL(constant_RAL(w, v), constant2D_RAL(h - 1, w, v))
    }
  }  

  case class RAL2D[T](data: MyList[Digit[MyList[Digit[T]]]], width: Int,
    get: (RAL2D[T], Int, Int) => T,
    put: (RAL2D[T], Int, Int, T) => RAL2D[T]
  )

  def RAL2D_create_constant[T](h: Int, w: Int, v: T) : RAL2D[T] = {
    val data = constant2D_RAL[T](h,w,v)
    RAL2D[T](data, w, RAL2D_get, RAL2D_put)
  }
  def RAL2D_create_from_list[T](lst: MyList[MyList[T]], w: Int) : RAL2D[T] = {
    val data = fromList2D_RAL[T](lst)
    RAL2D[T](data, w, RAL2D_get, RAL2D_put)
  }
  def RAL2D_get[T](arr: RAL2D[T], row: Int, col: Int): T = lookup2D_RAL(row, col, arr.data)
  def RAL2D_put[T](arr: RAL2D[T], row: Int, col: Int, obj: T): RAL2D[T] = {
    RAL2D(update2D_RAL(col, row, obj, arr.data), arr.width, arr.get, arr.put)
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
  case class World(map : RAL2D[Int], lambdaMan : LambdaMan, ghosts: MyList[Ghost], height: Int, width: Int)
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

  def addVisibleGhostToMap(ghosts : MyList[Ghost], map : RAL2D[Int]) : RAL2D[Int] = {
    if (ghosts == MyNil) return map
    var res = addVisibleGhostToMap(ghosts.cdr, map)
    var ghost = ghosts.car
    if (ghost.vitality != 2) {
      res = res.put(res, ghost.pos.y, ghost.pos.x, 10)
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
    // var map = Array2D_create[Int](width)
    // map.from_list(map, argWorld.map, 0)
    var map = RAL2D_create_from_list(argWorld.map, width)
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

    // var dist = Array2D_create[Int](width)
    // dist.valset(dist, width * height, -1)
    var dist = RAL2D_create_constant(height, width, -1)
    dist = dist.put(dist, myPos.y, myPos.x, 0)
    // var prev = Array2D_create[Int](width)
    // prev.valset(prev, width * height, -1)
    var prev = RAL2D_create_constant(height, width, -1)

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
      // debug(((y,x), map.get(map, y, x)))
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
                dist = dist.put(dist, yy, xx, currentDist + 1)
                queueX = push(queueX, xx)
                queueY = push(queueY, yy)
                prev = prev.put(prev, yy, xx, d)
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
    // var arr = RAL2D_create_constant(3,3,1)
    // debug(arr.get(arr,1,1))
    // arr = arr.put(arr,1,1,10)
    // debug(arr.get(arr,1,1))
    // return 0
    var map = MyList(
      MyList(1,1,2,1),
      MyList(1,1,1,0),
      MyList(1,1,1,0),
      MyList(0,2,1,0))
    var lambdaMan = LambdaMan(0, Point(1,1), 0)
    var ghosts = MyList(Ghost(0, Point(1,0), 1))
    var world = ArgWorld(map, lambdaMan, ghosts, 0)
    debug(step(0, world))
    return 0
  }
}

import java.io.PrintWriter

object sune2AIRAL extends AstCleanup {
  val asts = ???

  def main(args: Array[String]) {
    // var tmp = new sune2AIRAL()
    // tmp.myMain()
    // return
	  var codeList = Linker.compileAndLink(cleanAsts, "entryPoint")
    // println(CodeGen.dereferenceLabels(codeList).map(CodeGen.show).mkString("", "\n", ""))
    var p = new PrintWriter("code.txt")
    p.println(CodeGen.dereferenceLabels(codeList).map(CodeGen.show).mkString("", "\n", ""))
    p.close()
  }
}
