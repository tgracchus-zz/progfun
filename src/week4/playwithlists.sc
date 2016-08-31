object playwithlist {

  val fruit = List("apples", "oranges", "pears")
  val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))

  val x = 1
  val y = 2
  val xs = 3
  val yz = 4
  val zs = 5


  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
  }


}