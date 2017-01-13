object sfit extends App {
  // 1.
  def values(fun: (Int) => Int, low: Int, high: Int) = {
    (low to high).map(x => (x, fun(x)))
  }
  println(s"values(x => x*x, -5, 5) = ${values(x => x*x, -5, 5)}")

  // 2.
  println((-5 to 5).reduceLeft(_ max _))

  // 3.
  println((1 to 5).reduceLeft(_ * _))

  // 4.
  println((1 to 5).foldLeft(1)(_ * _))

  // 5.
  def largest(fun: (Int) => Int, inputs:Seq[Int]) = {
    inputs.reduceLeft(_ max fun(_))
  }
  println(largest(x => 10 * x - x * x, 1 to 10))

  // 6.
  def largestAt(fun: (Int) => Int, inputs:Seq[Int]) = {
    inputs.reduceLeft(_ max fun(_))
  }
  println(largest(x => 10 * x - x * x, 1 to 10))

  // 7.
  def adjustToPair(f: (Int, Int) => Int)(a: (Int, Int)) = f(a._1, a._2)
  println(adjustToPair(_ * _)((6, 7)))

  // 8.
  val a = Array("Hello", "World")
  val b = Array(5, 5)
  a.corresponds(b)(_.length == _)

  // 9.
  def corresponds2[A, B](a:Seq[A], b: Seq[B], p: (A, B) => Boolean): Boolean = {
    if (a == Seq.empty[A] || b == Seq.empty[B]) true
    else p(a.head, b.head) && corresponds2[A, B](a.tail, b.tail, p)
  }
  corresponds2(a, b, (x:String, y:Int) => x.length == y)

  def corresponds3[A, B](a:Seq[A], b: Seq[B], p: (A, B) => Boolean): Boolean = {
    (a zip b).map{ case(i, j) => p(i, j) }.reduce(_ & _)
  }
  println(corresponds3(a, b, (x:String, y:Int) => x.length == y))

  // 10.
  def unless(condition: => Boolean)(block: => Unit) {
    if (!condition) {
      block
    }
  }
  var x = 2
  unless(x == 0) {
    println(x)
  }
}
