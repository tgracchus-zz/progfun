object exercise {

  def factorial(x: Int): Int = {

    def factorialIter(acu: Int, n: Int): Int = {
      if (n==0) {
        acu
      } else {
        factorialIter(acu * n,n-1)
      }

    }

    factorialIter(1,x)
  }


  factorial(3)
  factorial(4)

}

