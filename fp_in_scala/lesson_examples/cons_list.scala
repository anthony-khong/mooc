trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def index(n: Int): T
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = true
  def index(n: Int) = if (n == 0) head else tail index (n - 1)
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
  def index(n: Int) = throw new NoSuchElementException("Index out of bounds")
}
