import util.Random
import java.awt.datatransfer._
import scala.collection.mutable._
import java.util.TimeZone._
import java.awt.datatransfer._

object sfti extends App {
  // 1. Write a code snippet that sets a to an array of n random integers between 0
  // (inclusive) and n (exclusive).
  val n = 10
  val sol1 = Seq.fill(n)(Random.nextInt(n))
  println(s"ex1 : $sol1")

  // 3. Repeat the preceding assignment, but produce a new array with the swapped
  // values. Use for/yield.
  var array = for (i <- 1 to 5) yield i
  println(array) 
  var swapRel = for (i <- 0 until array.length) yield { 
    if (i % 2 == 0)
      if (i+1 < array.length) array(i+1) 
      else array(i)
    else array(i-1)
  }
  println(s"ex3: $swapRel")

  // 4. Given an array of integers, produce a new array that contains all positive
  // values of the original array, in their original order, followed by all values that
  // are zero or negative, in their original order.
  var a4 = Array(-5, 1, -2, 3, 4, 5)
  var sol4 = (a4.filter(_ > 0) ++ a4.filter(_ <= 0)).toBuffer
  println(s"ex4 : $sol4")

  // 5. How do you compute the average of an Array[Double]?
  var array5 = Array(0, 1, 2, 3, 4, 5, 7)
  val sol5 = array5.sum.toFloat/array5.length
  println(s"ex5: $sol5")

  // 6. How do you rearrange the elements of an Array[Int] so that they appear in
  // reverse sorted order? How do you do the same with an ArrayBuffer[Int]?
  var array6 = Array(0, 1, 2, 3, 4, 5, 7)
  for (i <- 0 until array6.length/2) {
    var tmp = array6(i)
    array6(i) = array6(array6.length - i - 1)
    array6(array6.length - i - 1) = tmp
  }
  val sol6 = array6.mkString(" ")
  println(s"ex6: $sol6")

  // 7. Write a code snippet that produces all values from an array with duplicates
  // removed. (Hint: Look at Scaladoc.)
  var array7 = Array(0, 1, 0, 1, 4, 5, 7, 4)
  val sol7 = array7.distinct.mkString(", ")
  println(s"ex7 : $sol7")

  // 8. Rewrite the example at the end of Section 3.4, "Transforming Arrays," on
  // page 34 using the drop method for dropping the index of the first match. Look
  // the method up in Scaladoc.
  var array8 = ArrayBuffer(-5, 1, -2, 3, 4, 5)
  var indices = for (i <- 0 until array8.length if array8(i) < 0) yield i
  indices.drop(1)
  for (i <- indices.reverse) array8.remove(i)
  println(s"ex7 : $array8")

  // 9. Make a collection of all time zones returned by java.util.TimeZone.getAvailableIDs
  // that are in America. Strip off the "America/" prefix and sort the result.
  val prefix = "America/"
  val americaZone = getAvailableIDs().filter(_.startsWith(prefix)).map( (s) => s.stripPrefix(prefix))
  val sol8 = americaZone.mkString(", ")
  println(s"ex9: $sol8")

  // 10. Import java.awt.datatransfer._ and make an object of type SystemFlavorMap with
  // the call
  // val flavors = SystemFlavorMap.getDefaultFlavorMap().asInstanceOf[SystemFlavorMap]
  // Then call the getNativesForFlavor method with parameter DataFlavor.imageFlavor
  // and get the return value as a Scala buffer. (Why this obscure class? It's hard
  // to find uses of java.util.List in the standard Java library.)
  import java.awt.datatransfer._
  import scala.collection.JavaConverters._
  import scala.collection.mutable.Buffer
  val flavors = SystemFlavorMap.getDefaultFlavorMap().asInstanceOf[SystemFlavorMap]
  //> flavors  : java.awt.datatransfer.SystemFlavorMap = java.awt.datatransfer.Sy
  //| stemFlavorMap@54b24c03
  val nativesForFlavors = flavors.getNativesForFlavors(Array(DataFlavor.imageFlavor))
  //> nativesForFlavors  : java.util.Map[java.awt.datatransfer.DataFlavor,String]
  //|  = {java.awt.datatransfer.DataFlavor[mimetype=image/x-java-image;representa
  //| tionclass=java.awt.Image]=PNG}
  val sol10 = collection.JavaConversions.asScalaBuffer(new java.util.LinkedList(nativesForFlavors.values()))
  println(s"ex10: $sol10")
}
