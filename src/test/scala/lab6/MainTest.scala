package lab6

import org.junit._
import org.junit.Assert.assertEquals

class MainTest {
  import Main._

  trait TestMatrices {
    val r1 = List(1, 2, 3, 4, 5, 6, 7)
    val r2 = List(1, 2, 3, 4, 5, 6, 7, 8)


    val m1 = List(List(1, 4, 7), List(2, 5, 8), List(3, 6, 9))
    val m1transposed = List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))

    val m2 = List(List(1, 4), List(2, 5), List(3, 6))
    val m2transposed = List(List(1, 2, 3), List(4, 5, 6))


    val e1 = List(List())
    val e2 = List(List())
  }

  def equalRows(r1: Row, r2: Row): Boolean =
    if (r1.length != r2.length) false
    else if (r1.isEmpty) true
    else (r1.head == r2.head) && equalRows(r1.tail, r2.tail)

  def equalMatrices(m1: Matrix, m2: Matrix): Boolean =
    m1.zip(m2).map(e => equalRows(e._1, e._2)).reduceLeft(_ && _)

  @Test def `equal rows test`: Unit =
    new TestMatrices {
      Assert.assertEquals(false, equalRows(r1, r2))
    }

  @Test def `square matrix transpose`: Unit =
    new TestMatrices {
      Assert.assertEquals(true, equalMatrices(m1transposed, m1.transpose))
    }

  @Test def `rectangle matrix test`: Unit =
    new TestMatrices {
      Assert.assertEquals(true, equalMatrices(m2transposed, m2.transpose))
    }

  @Test def `different matrices`: Unit =
    new TestMatrices {
      Assert.assertEquals(false, equalMatrices(m2, m1.transpose))
    }

  @Test def `empty matrices`: Unit =
    new TestMatrices {
      Assert.assertEquals(true, equalMatrices(e1, e2.transpose))
    }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}

