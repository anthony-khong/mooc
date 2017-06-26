abstract class IntSet {
  def contains(x: Int): Boolean
  def include(x: Int): IntSet
  def union(other: IntSet): IntSet
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet)  extends IntSet {
  override def toString = "{" + left + elem + right + "}"

  def contains(x: Int) = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def include(x: Int) = {
    if (x < elem) new NonEmpty(elem, left include x, right)
    else if (x > elem) new NonEmpty(elem, left, right include x)
    else this
  }

  def union(other: IntSet) = ((left union right) union other) include elem
}

object Empty extends IntSet {
  override def toString = "_"
  def contains(x: Int) = false
  def include(x: Int) = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet) = other
}

