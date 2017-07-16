package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      heap <- oneOf(const(empty), genHeap)
    } yield insert(x, heap)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def toList(heap: H): List[A] = {
    if (isEmpty(heap)) Nil
    else findMin(heap) :: toList(deleteMin(heap))
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert1") = forAll { (h: H, a: Int) =>
    val elements = toList(insert(a, h))
    elements contains a
  }


  property("meld1") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) || isEmpty(h2)) true
    else {
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      val m = if (m1 < m2) m1 else m2
      findMin(meld(h1, h2)) == m
    }
  }

  property("meld2") = forAll{ h: H =>
    if (isEmpty(h)) true
    else findMin(h) == findMin(meld(h, empty))
  }

  property("findMin1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("deleteMin1") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

}
