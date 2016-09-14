object luistfun {

  def squareListS(xs: List[Int]): List[Int] =
    xs match {
      case Nil => Nil
      case y :: ys => (y * y) :: squareListS(ys)
    }

  //map
  def squareList(xs: List[Int]): List[Int] =
  xs map (y => y * y)


  val nums = List(2, -4, 5, 5, 7, 1)
  val fruits = List("apple", "pineapple", "organge", "banana")

  //filter
  nums.filter(x => x > 0)
  nums.filterNot(x => x > 0)
  nums partition (x => x > 0)

  //take and rop
  nums takeWhile (x => x > 0)
  nums dropWhile (x => x > 0)
  nums span (x => x > 0)

  pack(List("a", "a", "a", "b", "c", "c", "a"))

  List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }


  def encode[T](xs: List[T]): List[(T, Int)] = {
    pack(xs).map(list => (list.head, list.length))
  }


  encode(List("a", "a", "a", "b", "c", "c", "a"))


}