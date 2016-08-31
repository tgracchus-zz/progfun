/**
  * Created by ulises on 29/08/16.
  */

package week4covariance

trait List[+T] {

  def isEmpty: Boolean

  def head: T

  def tail: List[T]

  def singleton[U >: T](elem: U) = new Cons(elem, new Nil)

  def prepend[U >: T](elem: U) = new Cons(elem, this)

  def nth(position: Int)



}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {

  def isEmpty: Boolean = false

  def nth(position: Int): Unit = {

    if (this.isEmpty) throw new IndexOutOfBoundsException
    if (position == 0) {
      head
    } else
      tail.nth(position - 1)
  }
}

class Nil extends List[Nothing] {

  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.tail")

  def nth(position: Int): Unit = throw new IndexOutOfBoundsException("Nil.nth")
}



