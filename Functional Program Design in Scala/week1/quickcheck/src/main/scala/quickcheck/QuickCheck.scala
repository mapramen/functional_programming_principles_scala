package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for{
      x <- arbitrary[A]
      h <- genHeap
    } yield insert(x, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen4") = forAll { (list: List[A]) =>
    def heapFromList(list: List[A], heap: H): H = list match {
      case Nil => heap
      case x :: xs => heapFromList(xs, insert(x, heap))
    }
    
    def checkHeapOrder(heap: H, list: List[A]): Boolean = list match {
      case Nil => true
      case x :: xs => findMin(heap) == x && checkHeapOrder(deleteMin(heap), xs)
    }

    checkHeapOrder(heapFromList(list, empty), list.sorted(ord))
  }

  property("gen5") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    if (isEmpty(h1)){
      if (isEmpty(h2)){
        isEmpty(h)
      }
      else{
        findMin(h2) == findMin(h)
      }
    }
    else {
      if (isEmpty(h2)){
        findMin(h1) == findMin(h)
      }
      else{
        ord.min(findMin(h1), findMin(h2)) == findMin(h)
      }
    }
  }

}
