
object maps {
  val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
  val capitalOfCountry = Map("US" -> "Washington")

  capitalOfCountry("US")
  //capitalOfCountry("Andorra")

  capitalOfCountry get "Andorra"
  capitalOfCountry get "US"

  def showCapital(country: String) = capitalOfCountry.get(country) match {
    case Some(capital) => capital
    case None => "missing data"
  }

  showCapital("US")
  showCapital("Andorra")

  val fruit = List("apple", "pear", "orange", "pineapple")

  fruit sortWith (_.length < _.length)
  fruit.sorted

  Map(0 -> 5, 1 -> -2, 3 -> 1)

  class Poly(terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)

    //def +(other: Poly) = new Poly(terms ++ (other.terms map adjust))

    def +(other: Poly) = new Poly((other.terms foldLeft terms) (addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      terms + (exp -> (coeff + terms(exp)))
    }

    val terms = terms0 withDefaultValue 0.0

    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }

    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse)
        yield coeff + "x^" + exp) mkString " + "
  }

  val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
  val p2 = new Poly(Map(0 -> 2.0, 3 -> 7.0))
  p1 + p2
}