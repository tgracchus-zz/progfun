object implicitparam {


  def mbsort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y :: ys1) => if (lt(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
        }

      val (fst, snd) = xs splitAt n
      merge(mbsort(fst)(lt), mbsort(snd)(lt))
    }
  }


  mbsort(List("apple", "orange", "pineapple", "banana"))((x, y) => x.compareTo(y) < 0)

  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (x :: xs1, y :: ys1) => if (ord.lt(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
        }

      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  msort(List("apple", "orange", "pineapple", "banana"))




}