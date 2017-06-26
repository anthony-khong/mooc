object InsertSort {
  def main(args: Array[String]): Unit = {
    val list = List(5, 1, 5, 3, 0, 123)
    val sortedList = insertSort(list)
    sortedList.map(println)
  }

  def insertSort(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => insert(y, insertSort(ys))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case Nil => List(x)
    case y :: ys => if (x > y) y :: insert(x, ys) else x :: xs
  }
}
