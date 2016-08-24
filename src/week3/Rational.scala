package week3

/**
  * Created by ulises on 24/08/16.
  */
class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x,1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  val numer = x / gcd(x, y)
  val denom = y / gcd(x, y)

  def add(that: Rational) = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  }

  def neg: Rational = new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)

  def less(that: Rational) = this.numer * that.numer < that.numer * this.denom

  def max(that: Rational) = if (this.less(that)) that else this

  override def toString: String = numer + "/" + denom
}