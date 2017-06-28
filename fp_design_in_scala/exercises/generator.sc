import scala.math

trait Generator[+A] {
  self =>

  def generate: A

  def map[B](fn: A => B): Generator[B] = new Generator[B] {
    def generate = fn(self.generate)
  }

  def flatMap[B](fn: A => Generator[B]): Generator[B] = new Generator[B] {
    def generate = fn(self.generate).generate
  }
}

def integers = new Generator[Int] {
  val rand = new java.util.Random
  def generate = rand.nextInt
}

def booleans = for (n <- integers) yield n > 0

def single[T](x: T) = new Generator[T] { def generate = x }

def pairs = for (i <- integers; j <- integers) yield (i, j)

def unif(low: Int, high: Int) = for (n <- integers) yield {
  low + math.abs(n) % (high - low)
}

def chooseOne(xs: Int*) = for (ix <- unif(0, xs.length)) yield xs(ix)

def lists: Generator[List[Int]] = {
  def emptyLists = single(Nil)

  def nonEmptyLists = for (i <- integers; randList <- lists) yield i :: randList

  for {
    empty <- booleans
    list <- if (empty) emptyLists else nonEmptyLists
  } yield list

}

trait Tree
case class Inner(left: Tree, right: Tree) extends Tree {
  override def toString = "Inner(" + left + ", " + right + ")"
}
case class Leaf(x: Int) extends Tree {
  override def toString = "Leaf(" + x + ")"
}

def trees: Generator[Tree] = {
  def leaf = for (n <- integers) yield Leaf(n)
  def inner = for (left <- trees; right <- trees) yield Inner(left, right)
  for {
    isLeaf <- booleans;
    tree <- if (isLeaf) leaf else inner
  } yield tree
}
