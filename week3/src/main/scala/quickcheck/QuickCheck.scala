package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =

    for {
      k <- arbitrary[Int]
      m <- oneOf(const(empty), genHeap)
    } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("delMin1") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    h2 == empty
  }

  property("meldDelMin1") = forAll { (a: Int, b:Int)=>
    val h1 = insert(a, empty)
    val h2 = insert(b, empty)
    val h3 = meld(h1, h2)
    if (a < b) findMin(h3) == a else findMin(h3) == b

  }


  property("meldMinDelIns1") = forAll { (h1: H, h2: H) =>
    def sortH(ts: H, as: List[Int]): List[Int] = {
      if (isEmpty(ts)) as
      else findMin(ts) :: sortH(deleteMin(ts), as)
    }
    val meld1 = meld(h1, h2)
    val meld2 = meld(deleteMin(h1), insert(findMin(h1), h2))
    val xs1 = sortH(meld1, Nil)
    val xs2 = sortH(meld2, Nil)
    xs1 == xs2
  }
}
