val nums = Vector(1, 2, 3, -88)
val people = Vector("Bob", "James")



3 +: nums
nums :+ 3


val xs = Array(1, 2, 3, 44)
xs map (x => x * 2)

val s = "Hello World"

s filter (c => c.isUpper)

val r: Range = 1 until 5
val to: Range = 1 to 5

1 to 10 by 3
6 to 1 by -2


s exists (c => c.isUpper)
s forall (c => c.isUpper)

val pairs = List(1, 2, 3, 4) zip s
pairs unzip

s flatMap (c => List('.', c))

xs sum

xs max

def isPrime(n: Int): Boolean = {
  2 until n forall (d => n % 2 != 0)
}