object sum {


  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }

  sum(x => x, 3, 5)

  def sumF(f: Int => Int): (Int, Int) => Int = {
    def loop(a: Int, b: Int): Int = {
      if (a > b) 0
      else f(a) + loop(a + 1, b)
    }
    loop
  }


  def sumInts = sumF(x => x)

  sumF(x => x)(3, 5)
  sumInts(3, 5)
  sumF(x => x)


  def sumFS(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0 else f(a) + sumFS(f)(a + 1, b)
  }

  sumFS(x => x)(3,5)


}