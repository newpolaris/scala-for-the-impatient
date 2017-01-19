object sfti extends App {
  // 1.
  /**
   * Your Java Development Kit distribution has the source code for much of the JDK in the src.zip file.
   * Unzip and search for case labels (regular expression case [^:]+: ).
   * Then look for comments starting with // and containing [Ff]alls? thr to catch comments such as
   * // Falls through or // just fall thru .
   *
   * Assuming the JDK programmers follow the Java code convention, which requires such a comment,
   * what percentage of cases falls through?
   */
  import scala.io.Source
  import java.io.File
  import scala.util.matching.Regex

  def subdirs(dir:File): Array[String] = {
    val childDir = dir.listFiles.filter(_.isDirectory)
    val childFiles = dir.listFiles.filter(_.isFile)
    childFiles.map(_.toString) ++ childDir.flatMap(subdirs(_)).map(_.toString)
  }
  
  def countFilePattern(filepath: String) = {
    try {
      val source = Source.fromFile(filepath, "UTF-8")
      val lines = source.getLines.toArray
      val casePattern = "case [^:]+:".r
      val falThruPattern = "[Ff]alls? thr".r

      val patternCnt = for (line <- lines) yield {
        if (casePattern.findFirstIn(line) != None) 
          if (falThruPattern.findFirstIn(line) != None) 1 else 2
        else 0
      }
      (patternCnt.count(_ == 1), patternCnt.count(_ == 2))
    } catch {
      case e : Throwable => (0, 0)
    }
  }

  def getAllCase(path: String) = {
    val abspath = path.replaceFirst("^~", System.getProperty("user.home"))
    subdirs(new File(abspath)).par.map(countFilePattern).fold((0, 0)){ (a, b) => (a._1 + b._1, a._2 + b._2)}
  }
  val cnt = getAllCase("~/Downloads/openjdk-6-src-b27-26_oct_2012")
  println(cnt)

  // 2.
  def swap(a:(Int, Int)) = a match { case (c,d) => (d, c) }
  println(swap((1, 3)))

  // 3.
  def swap2(a:Array[Int]) = a match { case Array(c,d) => Array(d, c) }
  println(swap2(Array(1, 3)).mkString(","))

  // 4.
  sealed abstract class Item
  case class Article(desc:String, price: Double) extends Item
  case class Bundle(desc:String, disc: Double, items: Item*) extends Item
  case class Multiple(cnt:Int, items: Item*) extends Item

  val m = Multiple(10,
    Bundle("DVD pack", 2.0,
      Article("Dr No", 4.00),
      Article("Goldfinger", 6.00)
    ),
    Multiple(2,
      Article("Thunderbolt", 7.00)
    )
  )

  def price(item: Item): Double = item match {
    case Article(_, p) => p
    case Bundle(_, disc, items @ _*) => items.map(price).sum - disc
    case Multiple(cnt, items @ _*) => cnt * items.map(price).sum
  }
  println(price(m)) // 220.0

  // 5.
  def leafSum(l: List[Any]):Int = l.map(_ match {
    case l: List[Any] => leafSum(l)
    case n: Int => n
  }).sum
  println(leafSum(List(List(3, 8), 2, List(5))))

  def leafSum2(l: List[Any]):Int = l match {
    case (x: Int) :: xs => x + leafSum2(xs)
    case (x: List[Any]) :: xs => leafSum2(x) + leafSum2(xs)
    case _ => 0
  }
  println(leafSum2(List(List(3, 8), 2, List(5))))

  // 6.
  sealed abstract class BinaryTree
  case class Leaf(value: Int) extends BinaryTree
  case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree
  def leafSum3(tree: BinaryTree):Int = tree match {
    case Leaf(value) => value
    case Node(left, right) => leafSum3(left) + leafSum3(right)
  }
  println(leafSum3(Node(Node(Node(Leaf(3), Leaf(8)), Leaf(2)), Leaf(5))))

  // 7.
  sealed abstract class BinaryTree2
  case class Leaf2(value: Int) extends BinaryTree2
  case class Node2(trees: BinaryTree2*) extends BinaryTree2
  def leafSum4(tree: BinaryTree2):Int = tree match {
    case Leaf2(value) => value
    case Node2(trees @ _*) => trees.map(leafSum4).sum
  }
  println(leafSum4(Node2(Node2(Leaf2(3), Leaf2(8)), Leaf2(2), Leaf2(5))))

  // 8.
  sealed abstract class CalcTree
  case class LeafC(value: Int) extends CalcTree
  case class NodeC(op: Char, tree:CalcTree*) extends CalcTree

  def eval(tree: CalcTree):Int = tree match {
    case LeafC(v) => v
    case NodeC(op, tree @ _*) => op match {
      case '+' => tree.map(eval).reduceLeft(_ + _)
      case '-' => - (tree.map(eval).reduceLeft(_ + _))
      case '*' => tree.map(eval).reduceLeft(_ * _)
    }
  }
  println(eval(NodeC('+', NodeC('*', LeafC(3), LeafC(8)), LeafC(2), NodeC('-', LeafC(5)))))
  
  // 9.
  def sumList(l: List[Option[Int]]):Int = { 
    (for (Some(i) <- l) yield i).sum
  }
  println(sumList(List(Some(1), None, Some(2))))

  // 10.
  import scala.math._
  def f(x: Double) = if (x >= 0) Some(sqrt(x)) else None
  def g(x: Double) = if (x != 1) Some(1 / (x - 1)) else None
  type FT = Double => Option[Double]
  def compose(f: FT, g: FT): FT = { 
    (x:Double) => g(x) match {
      case Some(x) => f(x)
      case None => None
    }
  }
  val h = compose(f, g)
  println(h(2))
  println(h(1))
  println(h(0))
}
