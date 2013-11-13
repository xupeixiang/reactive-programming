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
  
  property("isEmpty") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(h) == false
  }
}
