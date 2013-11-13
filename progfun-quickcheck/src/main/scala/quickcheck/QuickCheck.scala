package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    ele <- arbitrary[Int]
    heap <- oneOf(value(empty), genHeap)
  } yield insert(ele, heap)
  

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
  
  property("delete") = forAll { a: Int =>
    val h = deleteMin(insert(a / 4, insert(a / 2, insert(a, empty))))
    findMin(h) == a / 2 
  }
  
  property("min2") = forAll { a: Int =>
    val h = insert(a / 4, insert(a / 2, insert(a, empty)))
    if (a < 0) findMin(h) == a else findMin(h) == a/4 
  }
  
  property("meld") = forAll { a: Int =>
    val h1 = insert(a / 2, insert(a, empty))
    val h2 = insert(a / 8, insert(a / 4, empty))
    val h = meld(h1, h2)
    if (a < 0) findMin(h) == a else findMin(h) == a/8
  }
}
