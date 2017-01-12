object sfti extends App {
 // 1.
 /*
  * Operator precedence fixed in the Scala Reference 
  * 6.12.3 Infix Operations by the first character in the operator
  
   scala> 3 + 4 -> 5
   res0: (Int, Int) = (7,5)

   scala> 3 -> 4 + 5
   <console>:12: error: type mismatch;
   found   : Int(5)
   required: String
   3 -> 4 + 5
            ^
   scala> 3 -> (4 + 5)
   res2: (Int, Int) = (3,9)

  */
  // 2.
  implicit class PowerInt(i: BigInt) {
    def ** (b: Int): BigInt = i.pow(b)
  }
  // scala> 2*BigInt(5) ** 2
  // res8: BigInt = 100

  // 3.
  class Fraction(n: Int, d: Int) {
    private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d)
    private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d)
    override def toString = num + "/" + den
    def sign(a: Int) = if (a > 0) 1 else if (a < 0) -1 else 0
    def gcd(a: Int, b: Int): Int = if (b == 0) math.abs(a) else gcd(b, a % b)
    def +(other: Fraction):Fraction = { new Fraction(num*other.den + other.num*den, den*other.den) }
    def -(other: Fraction):Fraction = { new Fraction(num*other.den - other.num*den, den*other.den) }
    def *(other: Fraction):Fraction = { new Fraction(other.num*num, other.den*den) }
    def /(other: Fraction):Fraction = { new Fraction(other.num*den, other.den*num) }
  }

  val a = new Fraction(1, 3)
  val b = new Fraction(2, 3)

  println(s"$a + $b = ${a+b}")
  println(s"$a * $b = ${a*b}")
  println(s"$a / $b = ${a/b}")
  println(s"$a - $b = ${a-b}")

  /**
   * 4. Implement a class Money with fields for dollars and cents. Supply + , - operators as well as
   * comparison operators == and < .
   *
   * For example, Money(1, 75) + Money(0, 50) == Money(2, 25) should be true .
   *
   * Should you also supply * and / operators? Why or why not?
   */
  // NOTE: object apply 로 2 개 인자 생성자를 옮기는게 더 보기 좋다.
  // https://github.com/Gerhut/scala-for-the-impatient/blob/master/Chapter11/4.scala
  class Money(d: Int, c: Int) {
    private val cents = d*100 + c
    def this(c: Int) { this(0, c) }
    def this(c: Double) { this(0, c.toInt) }
    override def toString() = { s"${sign()}${cents/100}.${cents%100} dollar" }
    def sign():String = { if (cents > 0) "" else "-" }
    def +(that: Money):Money = { new Money(cents + that.cents) }
    def -(that: Money):Money = { new Money(cents - that.cents) }
    def ==(that: Money):Boolean = { cents == that.cents }
    def <(that: Money):Boolean = { cents < that.cents }
    def *(factor: Double):Money = { new Money(cents.toDouble*factor) }
    def /(factor: Double):Money = { new Money(cents.toDouble/factor) }
  }
  val ma = new Money(10, 5);
  val mb = new Money(3, 95);
  println(s"$ma + $mb = ${ma + mb}")
  println(s"$ma - $mb = ${ma - mb}")
  println(s"$ma == $ma = ${ma == ma}")

  // 5.
  // NOTE: "||" 파싱을 한다면 space 랑 " 모두 걸러내야 한다.
  // class 를 이용하는 편이 더 낫다
  class Table() {
    import scala.collection.mutable.ArrayBuffer
    var matrix = ArrayBuffer.fill(1, 0)("")
    def |(term:String) = {
      matrix.last += term
      this
    }
    def ||(term:String) = {
      matrix.append(ArrayBuffer(term))
      this
    }
    override def toString() = {
      val content = (for(row <- matrix) yield row.mkString("<tr><td>", "</td><td>", "</td><tr>"))
      content.mkString("<table>", "", "</table>")
    }
  }

  object Table {
    def apply() = new Table
  }

  println(s"Table Text : ${Table() | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM, .NET"}")

  // 6.
  class ASCIIArt(var art: String) {
    def |(that:ASCIIArt) = {
      import scala.math.max
      val maxWidth = art.split("\n").map(_.length).reduce(_ max _)
      val artSet = art.split("\n").zip(that.art.split("\n"))
      var merged = artSet.map { case (x, y) => x + " "*(maxWidth - x.length) + y }.mkString("\n")
      new ASCIIArt(merged)
    }
    def +(that:ASCIIArt) = {
      new ASCIIArt(art ++ "\n" ++ that.art)
    }
    override def toString() = {
      art 
    }
  }
  val kitty = new ASCIIArt(""" /\_/\
( ' ' )
(  -  )
 | | |
(__|__)""")
val talk = new ASCIIArt("""   -----
 / Hello \
<  Scala |
 \ Coder /
   -----
""")
  println(kitty+talk)
  println(kitty|talk)

  // 7.
  class BitSequence() {
    private var low:Long = 0
    private var high:Long = 0
    def apply(idx:Int) = {
      if (idx < 32) low & (1 << idx)
      else high & (1 << (idx - 32))
    }
    def update(idx:Int, bit:Int) = bit match {
      case 1 => { 
        if (idx < 32) low |= (1 << idx) 
        else high |= (1 << idx)
      }
      case 0 => {
        if (idx < 32) low &= ~(1 << idx) 
        else high &= ~(1 << idx) 
      }
    }
    override def toString() = {
      val l = (for (i <- 0 to 32) yield (low & (1 << i)) > 0)
      val h = (for (i <- 0 to 32) yield (high & (1 << i)) > 0)
      (l++h).reverse.map(if (_) "1" else "0").mkString("")
    }
  }
  var bit = new BitSequence()
  bit(1) = 1
  bit(1) = 0
  bit(4) = 1
  println(s"bit(10010) = ${bit}")

  // 8.
  class Matrix(val r:Int, val c:Int, in:Array[Int]) {
    private var data = in
    def +(that: Matrix) = {
      new Matrix(r, c, data.zip(that.data).map{ case (x, y) => x + y})
    }
    def *(factor: Int) = {
      new Matrix(r, c, data.map( _ * factor ))
    }
    def mat(row:Int, col:Int):Int = {
      data(row*c + col)
    }
    def *(that: Matrix) = {
      var ret = new Matrix(r, that.c, Array.fill(r*that.c)(0)) 
      for (i <- 0 until r;
           j <- 0 until that.c;
           k <- 0 until c) {
        val a = mat(i, k)
        val b = that.mat(k, j)
        ret.data(i*r+j) += a*b
      }
      ret
    }
    override def toString() = {
      data.grouped(r).map(_.mkString(" ")).mkString("[",";","]")
    }
  }
  var m = new Matrix(3, 3, Array(1, 0, 0, 0, 1, 0, 0, 0, 1))
  println(s"m = $m")
  println(s"m + m = ${m+m}")
  println(s"m * m = ${m*m}")

  // 9.
  object RichFile {
    def unapply(path:String) = {
      val re = """(.+)/(.+)\.(.+)""".r
      val re(p, n, e) = path
      Some(p, n , e)
    }
  }
  val RichFile(path, name, ext) = "/hello/world/example/too.time.consuming"
  println(s"$path $name $ext")

  // 10.
  object RichFile2 {
    def unapplySeq(path:String): Option[Seq[String]] = {
      Some(path.split('/'))
    }
  }
  // NOTE: "path2 @ _*"
  // HELP: http://stackoverflow.com/questions/2359014/scala-operator
  val RichFile2(path2 @ _*) = "/hello/wo.rld/example/too.time.consuming"
  println(s"$path2")
}
