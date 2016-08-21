object session {

  def abs(x: Double) = if (x < 0) -x else x

  def sqrt(x: Double) = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1)

  }

  sqrt(2)
  sqrt(3)
  sqrt(4)
  sqrt(1e-6)
  sqrt(1e60)

  var it1: Double = (1 + 1e-6 / 1) / 2
  var it2 = (it1 + 1e-6 / it1) / 2
  var it3 = (it2 + 1e-6 / it2) / 2
  var it4 = (it3 + 1e-6 / it3) / 2
  var it5 = (it4 + 1e-6 / it4) / 2
  var it6 = (it5 + 1e-6 / it5) / 2
  var it7 = (it6 + 1e-6 / it6) / 2

}