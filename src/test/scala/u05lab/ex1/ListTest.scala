package u05lab.ex1

import org.junit.Test
import org.junit.Assert.*

class ListTest:

  val l: List[Int] = List(1 ,2 ,3 ,4)

  @Test
  def testZipRight(): Unit =
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), l.zipRight)

  @Test
  def testPartition(): Unit =
    assertEquals((List(2, 4), List(1, 3)), l.partition(_ % 2 == 0))

  @Test
  def testSpan(): Unit =
    assertEquals((List(1), List(2, 3, 4)),l.span(_ % 2 != 0))
    assertEquals((List(1, 2), List(3, 4)), l.span(_ < 3))

  @Test
  def testReduce(): Unit =
    assertEquals(10, l.reduce(_ + _))

  @Test
  def testTakeRight(): Unit =
    assertEquals(List(2, 3, 4), l.takeRight(3))

  @Test
  def TestCollect(): Unit =
    val f: PartialFunction[Int, Int] =
      case x if (x % 2) == 0 => x * 10
    assertEquals(List(20, 40), l.collect(f))


