def arraySize(lst : MyList[Int]) : Int = {
  if (lst == MyNil) {
    return 0
  }
  return 1 + arraySize(lst.cdr)
}

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
