object playlist {
  val xs = List(1, 4, 6, 8)
  val ys = List(9, 10, 11, 12)

  //Util methods
  xs.length
  xs.last
  xs.init


  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => List()
    case y :: ys => y :: init(ys)
  }


  init(xs)

  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => xs
    case y :: ys => reverse(ys) ++ List(y)
  }

  reverse(xs)


  def removeAt[T](n: Int, xs: List[T]) = (xs take (n)) ::: (xs drop (n+1))

  val removeValue = removeAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)


  xs take 2
  xs drop 3
  xs(2)

  //Create
  xs ++ ys

  xs.reverse
  xs updated(1, 12)


  //Finding
  xs indexOf 1
  xs contains 6

  //Sublist


}