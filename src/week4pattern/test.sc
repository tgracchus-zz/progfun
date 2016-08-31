object test {

  trait Expr {

    def eval(): Int = this match {
      case Number(n) => n
      case Sum(l, r) => l.eval() + r.eval
    }

    def show(): String = this match {
      case Number(n) => n.toString
      case Sum(e1, e2) => e1.show() + " + " + e2.show()
      case Var(x) => x
      case Prod(l, r) => sumOfProd(l) + " * " + sumOfProd(r)

    }

    private def sumOfProd(expr: Expr): String = {
      expr match {
        case Sum(l, r) => "(" + l.show() + " + " + r.show() + ")"
        case _ => expr.show()
      }
    }
  }

  case class Number(n: Int) extends Expr

  case class Sum(e1: Expr, e2: Expr) extends Expr

  case class Var(x: String) extends Expr

  case class Prod(e1: Expr, e2: Expr) extends Expr


  Sum(Prod(Number(2), Var("x")), Var("y")).show()
  Prod(Sum(Number(2), Var("x")), Var("y")).show()

}