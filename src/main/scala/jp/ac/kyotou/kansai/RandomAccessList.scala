package jp.ac.kyotou.kansai

@gccCode
class RandomAccessList extends Support {
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
  def constant2D_RAL(h: Int, w: Int, v: T): MyList[Digit[MyList[Digit[T]]]] = {
    if (h == 0) {
      return MyNil
    } else {
      return cons_RAL(constant_RAL(w, v), constant2D_RAL(h - 1, w, v))
    }
  }

  def ent(): Int = {
    var ral: MyList[Digit[Int]] = cons_RAL(1, cons_RAL(2, cons_RAL(3, cons_RAL(4, MyNil))))
    debug(head_RAL(ral))
    debug(head_RAL(tail_RAL(ral)))
    debug(head_RAL(tail_RAL(tail_RAL(ral))))
    debug(head_RAL(tail_RAL(tail_RAL(tail_RAL(ral)))))
    debug(size_RAL(ral))
    ral = update_RAL(0, 5, ral)
    ral = update_RAL(1, 6, ral)
    ral = update_RAL(2, 7, ral)
    debug(head_RAL(ral))
    debug(head_RAL(tail_RAL(ral)))
    debug(head_RAL(tail_RAL(tail_RAL(ral))))
    debug(head_RAL(tail_RAL(tail_RAL(tail_RAL(ral)))))
    ral = cons_RAL(10, ral)
    debug(lookup_RAL(0, ral))
    debug(lookup_RAL(1, ral))
    debug(lookup_RAL(2, ral))
    debug(lookup_RAL(3, ral))
    debug(lookup_RAL(4, ral))
    debug(size_RAL(ral))

    // RAL is generic
    var ral2: MyList[Digit[(Int, Int)]] = cons_RAL((1, 1), cons_RAL((2, 2), cons_RAL((3, 3), MyNil)))
    debug(lookup_RAL(0, ral2))
    debug(lookup_RAL(0, ral2)._1)
    debug(lookup_RAL(0, ral2)._2)
    debug(lookup_RAL(1, ral2))
    debug(lookup_RAL(2, ral2))
    ral2 = update_RAL(1, (5, 5), ral2)
    debug(lookup_RAL(1, ral2))

    // We can convert lists to RAL.
    var l: MyList[Int] = MyList(1, 2, 3, 4, 5)
    var ral3 = fromList_RAL(l)
    debug(lookup_RAL(1, ral3)) // should be 2
    return 0
  }
}

import java.io.PrintWriter

object RandomAccessList extends AstCleanup {
  val asts = ???

  def main(args: Array[String]) {
    var c = new RandomAccessList()
    c.ent()

    var code = Linker.compileAndLink(cleanAsts, "ent")
    //println(CodeGen.dereferenceLabels(code).map(CodeGen.show).mkString("", "\n", ""))

    var p = new PrintWriter("code.txt")
    p.println(CodeGen.dereferenceLabels(code).map(CodeGen.show).mkString("", "\n", ""))
    p.close()
  }
}
