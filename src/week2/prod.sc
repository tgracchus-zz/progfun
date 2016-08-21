object prod {


  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1 else f(a) * product(f)(a + 1, b)
  }

  product(x => x)(3, 4)


  def factorial(n: Int): Int = {
    product(x => x)(1, n)
  }

  factorial(5)


  def mapReduce(map: Int => Int, reduce: (Int, Int) => Int, unit: Int)(a: Int, b: Int): Int = {
    if (a > b) unit else reduce(map(a), mapReduce(map, reduce, unit)(a + 1, b))
  }

  mapReduce(x => x, (a, b) => a * b, 1)(2, 3)

  def productF(f: Int => Int)(a: Int, b: Int): Int =   mapReduce(x => x, (a, b) => a * b, 1)(2, 3)
  productF(x => x)(3, 4)

}