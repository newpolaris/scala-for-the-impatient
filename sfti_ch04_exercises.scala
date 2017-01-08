object sfti extends App {
  // 1. Set up a map of prices for a number of gizmos that you covet. Then produce
  // a second map with the same keys and the prices at a 10 percent discount.
  val gizmos = Map("iPod" -> 20000, "iPhone" -> 45000, "iPad" -> 30000, "iMac" -> 85000)
  val discounted = for ((name, price) <- gizmos) yield (name, 0.9*price)
  println(s"ex1 : $discounted")

  // 2. Write a program that reads words from a file. Use a mutable map to count
  // how often each word appears. To read the words, simply use a java.util.Scanner:
  // val in = new java.util.Scanner(java.io.File("myfile.txt"))
  // while (in.hasNext()) process in.next()
  // Or look at Chapter 9 for a Scalaesque way.
  // At the end, print out all words and their counts.
  val fileName = "README"
  var in = new java.util.Scanner(new java.io.File(fileName))
  var counter = scala.collection.mutable.Map[String, Int]()
  while (in.hasNext()) {
    val key = in.next()
    val count = counter.getOrElse(key, 0)
    counter(key) = count+1
  }
  println(s"ex2 : $counter")

  // 3. Repeat the preceding exercise with an immutable map.
  in = new java.util.Scanner(new java.io.File(fileName))
  var imCounter = Map[String, Int]()
  while (in.hasNext()) {
    val key = in.next()
    var count = imCounter.getOrElse(key, 0)
    imCounter += (key -> (count+1))
  }
  println(s"ex3 : $imCounter")

  // 4. Repeat the preceding exercise with a sorted map, so that the words are
  // printed in sorted order.
  in = new java.util.Scanner(new java.io.File(fileName))
  var sortedCounter = scala.collection.mutable.SortedMap[String, Int]()
  while (in.hasNext()) {
    val key = in.next()
    if (sortedCounter.contains(key))
      sortedCounter(key) += 1
    else 
      sortedCounter(key) = 1
  }
  println(s"ex4 : $sortedCounter")

  // 5. Repeat the preceding exercise with a java.util.TreeMap that you adapt to the
  // Scala API.
  import scala.collection.JavaConversions.mapAsScalaMap
  val tMap: scala.collection.mutable.Map[String, Int] = new java.util.TreeMap[String, Int]
  //> tMap  : scala.collection.mutable.Map[String,Int] = Map()
  def tProcess(s: String) = {
    val c = tMap.getOrElse(s, 0);
    tMap(s) = c + 1
  }                                               //> tProcess: (s: String)Unit

  in = new java.util.Scanner(new java.io.File(fileName))
  while(in.hasNext()) tProcess(in.next())

  // 6. Define a linked hash map that maps "Monday" to java.util.Calendar.MONDAY, and
  // similarly for the other weekdays. Demonstrate that the elements are visited
  // in insertion order.
  import java.util.Calendar._
  var days = scala.collection.mutable.LinkedHashMap("Monday" -> MONDAY)
  days += ("Tuesday" -> TUESDAY, "Wednesday" -> WEDNESDAY)
  days += ("Thursday" -> THURSDAY, "Friday" -> FRIDAY)
  days += ("Saturday" -> SATURDAY, "Sunday" -> SUNDAY)
  val dayStr = for ((str, _) <- days) yield days
  println(s"ex5 : $dayStr")

  // 7. Print a table of all Java properties, like this:
  import scala.collection.JavaConversions.propertiesAsScalaMap
  val jprops: scala.collection.Map[String, String] = System.getProperties()
  val maxWord = jprops.keys.map(t => t.length).reduceLeft(_ max _)
  for ((k, v) <- jprops) println(k.padTo(maxWord, ' ') + " | " +v)

  // 8. Write a function minmax(values: Array[Int]) that returns a pair containing the
  // smallest and largest values in the array.
  val a8 = Array(0, 1, 2, 3, 4, 5, 9, 6, 4, 9)
  // NOTE: better solution -> values.min, values.max
  def minmax(values: Array[Int]) = {
    (values.reduceLeft(_ min _), values.reduceLeft(_ max _))
  }
  val pair = minmax(a8)
  println(s"ex8 : $pair")
  
  // 9. Write a function lteqgt(values: Array[Int], v: Int) that returns a triple containing
  // the counts of values less than v, equal to v, and greater than v.
  val a9 = Array(0, 1, 2, 3, 4, 3, 4, 5, 0, 5, 9, 6, 4, 9)
  // NOTE: better solution -> values.count(_ < v)
  def lteqgt(values: Array[Int], v:Int) = {
    val lt = values.filter(_ < v)
    val eq = values.filter(_ == v)
    (lt.length, eq.length, (values.length-lt.length-eq.length))
  }
  val numCnt = lteqgt(a9, 4)
  println(s"ex9 : $numCnt")

  // 10. What happens when you zip together two strings, such as "Hello".zip("World")?
  // Come up with a plausible use case.
  val zipStr = "Hello".zip("World")
  println(s"ex10 : $zipStr")
}
