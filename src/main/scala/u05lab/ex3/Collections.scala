package u05lab.ex3

import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils:
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]]:
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] =
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

@main def checkPerformance: Unit =

  import collection.*
  import PerformanceUtils.*

  val testValues = (1 to 10000000)

  /* Linear sequences: List, ListBuffer */
  lazy val list = testValues.toList
  lazy val lazyList: LazyList[Int] = LazyList.from(testValues)
  lazy val listBuffer: ListBuffer[Int] = mutable.ListBuffer.from(testValues)
  println("Linear Sequences -----------------------")
  println("------- Create operation")
  measure("List")(list)
  measure("LazyList")(lazyList)
  measure("ListBuffer")(listBuffer)
  println("------- Size operation")
  measure("List")(list.size)
  measure("LazyList")(lazyList.size)
  measure("ListBuffer")(listBuffer.size)
  println("------- Access Tail operation")
  measure("List")(list.tail)
  measure("LazyList")(lazyList.tail)
  measure("ListBuffer")(listBuffer.tail)
  println("------- Append operation")
  measure("List")(list :+ 0)
  measure("LazyList")(lazyList :+ 0)
  measure("ListBuffer")(listBuffer += 0)
  println("------- Remove operation")
  measure("ListBuffer")(listBuffer -= 0)
  println("------- Update operation")
  measure("List")(list.updated(1, 0))
  measure("LazyList")(lazyList.updated(1, 0))
  measure("ListBuffer")(listBuffer(1) = 0)
  println("-----------------------------------------")

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  lazy val vector = testValues.toVector
  lazy val array = testValues.toArray
  lazy val arrayBuffer = mutable.ArrayBuffer.from(testValues)
  println("\nIndexed Sequences -----------------------")
  println("------- Create operation")
  measure("Vector")(vector)
  measure("Array")(array)
  measure("ArrayBuffer")(arrayBuffer)
  println("------- Size operation")
  measure("Vector")(vector.size)
  measure("Array")(array.size)
  measure("ArrayBuffer")(arrayBuffer.size)
  println("------- Access Tail operation")
  measure("Vector")(vector.tail)
  measure("Array")(array.tail)
  measure("ArrayBuffer")(arrayBuffer.tail)
  println("------- Append operation")
  measure("Vector")(vector :+ 0)
  measure("Array")(array  :+ 0)
  measure("ArrayBuffer")(arrayBuffer  += 0)
  println("------- Remove operation")
  measure("ArrayBuffer")(arrayBuffer -= 0)
  println("------- Update operation")
  measure("Vector")(vector.updated(1, 0))
  measure("Array")(array.updated(1, 0))
  measure("ArrayBuffer")(arrayBuffer(1) = 0)
  println("-----------------------------------------")

  /* Sets */
  lazy val set = testValues.toSet
  lazy val mutableSet = mutable.Set.from(testValues)
  println("\nSets -----------------------")
  println("------- Create operation")
  measure("Set")(set)
  measure("MutableSet")(mutableSet)
  println("------- Size operation")
  measure("Set")(set.size)
  measure("MutableSet")(mutableSet.size)
  println("------- Access Tail operation")
  measure("Set")(set.tail)
  measure("MutableSet")(mutableSet.tail)
  println("------- Append operation")
  measure("MutableSet")(set + 0)
  measure("MutableSet")(mutableSet += 0)
  println("------- Remove operation")
  measure("MutableSet")(set - 1)
  measure("MutableSet")(mutableSet -= 0)
  println("-----------------------------------------")

  /* Maps */
  lazy val map = testValues.map(x => (x, x)).toMap
  lazy val mutableMap: mutable.Map[Int, Int] = mutable.Map.from(testValues.map(x => (x, x)))
  println("\nMaps -----------------------")
  println("------- Create operation")
  measure("Map")(map)
  measure("MutableMap")(mutableMap)
  println("------- Size operation")
  measure("Map")(map.size)
  measure("MutableMap")(mutableMap.size)
  println("------- Access operation")
  measure("Map")(map(100))
  measure("MutableMap")(mutableMap(100))
  println("------- Append operation")
  measure("Map")(map + (0 -> 0))
  measure("MutableMap")(mutableMap += (0 -> 0))
  println("------- Remove operation")
  measure("Map")(map - 1)
  measure("MutableMap")(mutableMap -= 0)
  println("-----------------------------------------")
