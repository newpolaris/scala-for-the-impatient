object sfti extends App {
  // 1. 
  import scala.collection.mutable.SortedSet
  import scala.collection.mutable.Map
  import scala.collection.immutable.{Map => NMap}

  def indexes(str: String) = {
    var ret = Map[Char, SortedSet[Int]]() 
    for ((c,i) <- str.toList.zipWithIndex) {
      ret(c) = ret.getOrElse(c, SortedSet[Int]()) + i
    } 
    ret
  }
  println(indexes("Mississippi"))

  // 2.
  def indexes2(str: String) = 
    (NMap[Char, SortedSet[Int]]() /: str.toList.zipWithIndex) {
      case (m, (c, i)) => m + (c -> (m.getOrElse(c, SortedSet[Int]()) + i))
    }
  println(indexes2("Mississippi"))

  // 3.
  import scala.collection.mutable.LinkedList
  def remove0(link: LinkedList[Int]): LinkedList[Int] = link match {
    case LinkedList(0) => LinkedList.empty
    case LinkedList(0, _*) => remove0(link.next)
    case LinkedList(h, _*) => LinkedList(h) ++ remove0(link.next)
  }
  var link = LinkedList(1, 2, 0, 3, 4, 0)
  println(remove0(link))

  def remove1(link: LinkedList[Int]) { 
    link match {
      case LinkedList(0) => link.next = link
      case LinkedList(0, _*) => {
        link.elem = link.next.elem
        link.next = link.next.next
        remove1(link)
      }
      case LinkedList(h, _*) => remove1(link.next)
    }
  }
  remove1(link)
  println(link)

  // 4.
  def reduceString(list: Array[String], m: Map[String, Int]) = {
    list.flatMap(m.get(_))
  }
  println(reduceString(Array("Tom", "Fred", "Harry"), Map("Tom" -> 3, "Dick" -> 4, "Harry" -> 5)).mkString("[", " ", "]"))

  // 5.
  def mkStr[T](arr: Seq[T], pre: String, mid: String, post: String) = {
    pre + arr.map(_.toString).reduceLeft(_ + mid + _) + post
  }
  println(mkStr(Array("hello, word"), "[", ", ", "]"))

  // 6.
  val intList = List(1, 2, 3, 4, 5)
  println(intList)
  println((intList :\ List[Int]())(_ :: _))
  println((List[Int]() /: intList)(_ :+ _))
  println((intList :\ List[Int]())((x,y) => y :+ x))
  println((List[Int]() /: intList)((x,y) => y :: x))

  // 7.
  println((List(1,2,3) zip List(3,2,1)) map Function.tupled(_ * _))

  // 8.
  def arrayGroup(a: Array[Int], numCol: Int) = a.grouped(numCol).toArray
  println(arrayGroup((1 to 9).toArray, 3).mkString(", "))

  // 9.
  /* 
   * <console>:11: warning: trait SynchronizedMap in package mutable is deprecated (since 2.11.0): Synchronization via traits is deprecated as it is inherently unreliable. Consider java.util.concurrent.ConcurrentHashMap as an alternative.
   *    var fre = new scala.collection.mutable.HashMap[Char, Int] with scala.collection.mutable.SynchronizedMap[Char, Int]
   */
  val eList = (1 to 123456) map (_ => 'e')
  var fre = new scala.collection.mutable.HashMap[Char, Int] with scala.collection.mutable.SynchronizedMap[Char, Int]
  eList.par.foreach( c => fre(c) = fre.getOrElse(c, 0) + 1 )
  println(fre) // 356

  // from - https://github.com/BasileDuPlessis/scala-for-the-impatient/blob/master/src/main/scala/com/basile/scala/ch13/Ex09.scala
  import java.util.concurrent.atomic.AtomicInteger
  import scala.collection.JavaConversions.mapAsScalaConcurrentMap
  val fre2: scala.collection.concurrent.Map[Char, AtomicInteger] = new java.util.concurrent.ConcurrentHashMap[Char, AtomicInteger]
  eList.par.foreach( c => {
    val zero = new AtomicInteger(0)
    val v = fre2.putIfAbsent(c, zero).getOrElse(zero)
    v.incrementAndGet
  })
  println(fre2)

  // http://stackoverflow.com/questions/35373144/does-concurrenthashmap-need-synchronization-when-incrementing-its-values
  val fre3: scala.collection.concurrent.Map[Char, Int] = new java.util.concurrent.ConcurrentHashMap[Char, Int]
  type MM = scala.collection.concurrent.Map[Char, Int]
  def inc(acc: MM, c: Char) {
  }
  eList.par.foreach( c => {
    def h {
      while (true) {
        val v = fre3.putIfAbsent(c, 0).getOrElse(0)
         if (fre3.replace(c, v, v+1)) 
          return
      }
    }
    h
  })
  println(fre3)

  // 10.
  type HM = scala.collection.immutable.HashMap[Char, Int]
  val fre4 = eList.par.aggregate(new HM())(
    (x,c) => x + (c -> (x.getOrElse(c, 0) + 1)),
    (m1, m2) => m2.map{ case (k,v) => k -> (v + m1.getOrElse(k,0))} 
  )
  println(fre4)
}
