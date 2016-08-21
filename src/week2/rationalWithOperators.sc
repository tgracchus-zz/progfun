object rational {

  class Rational(x: Int, y: Int) {
    require(y != 0, "denominator must be nonzero")

    def this(x: Int) = this(x,1)

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    val numer = x / gcd(x, y)
    val denom = y / gcd(x, y)

    def +(that: Rational) = {
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom)

    }

    def unary_- : Rational = new Rational(-numer, denom)

    def -(that: Rational) = this + -that

    def <(that: Rational) = this.numer * that.numer < that.numer * this.denom

    def max(that: Rational) = if (this < that) that else this

    override def toString: String = numer + "/" + denom
  }


  val x = new Rational(1, 2)
  val y = new Rational(2, 3)
  x + y

  val a = new Rational(1, 3)
  val b = new Rational(5, 7)
  val c = new Rational(3, 2)
  a - b
  a < b
  a max b


}