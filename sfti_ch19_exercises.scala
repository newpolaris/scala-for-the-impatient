object sfit extends App {
  import scala.util.parsing.combinator._
  import language.postfixOps

  // 1, 2
  class ExprParser extends RegexParsers {
    val number = "[0-9]+".r
    def expr: Parser[Int] = term ~ rep(
      ("+" | "-") ~ term ^^ {
        case "+" ~ t => t
        case "-" ~ t => -t
      }) ^^ { case t ~ r => t + r.sum }
    def term: Parser[Int] = pow ~ rep(
      ("*" | "/" | "%") ~ pow) ^^ {
        case f ~ Nil => f
        case f ~ termList => termList.foldLeft(f) {
          case (x, "*" ~ y) => x * y 
          case (x, "/" ~ y) => x / y 
          case (x, "%" ~ y) => x % y 
        }
    }
    def pow: Parser[Int] = factor ~ rep(
      "^" ~> factor) ^^ {
        case f ~ Nil => f
        case f ~ powList => (f :: powList).foldRight(1) {
          case (y, x) => BigInt(y).pow(x).toInt
        }
    }
    def factor: Parser[Int] = number ^^ { _.toInt } | "(" ~> expr <~ ")"
  }

  val parser = new ExprParser
  val result = parser.parseAll(parser.expr, "(4^2^3)")
  if (result.successful) println(result.get)

  // 3.

}
