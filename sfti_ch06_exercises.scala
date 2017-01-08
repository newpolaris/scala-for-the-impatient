object sfti extends App {
  // 1.
  object Conversions {
    def inchesToCentimeters(in: Double) = 2.54*in
    def gallonsToLiters(gal: Double) = 3.78541 * gal
    def milesToKilometers(mi: Double) = 1.60934 * mi

  }

  // 2.
  class UnitConversion(val factor: Double) {
    def apply(value: Double) = factor * value
  }
  object InchesToCentimeters extends UnitConversion(2.54)

  println(s"1.0 inch = ${InchesToCentimeters(1.0)} cm")
  println(s"1.0 gallon = ${InchesToCentimeters(1.0)} liter")

  // 3.
  class Origin extends java.awt.Point {}

  // 4. 
  class Point private (x: Int, y: Int) { override def toString = s"$x, $y" }
  object Point {
    def apply(x: Int, y: Int) = new Point(x, y)
  }

  println(s"Point(3, 4) = ${Point(3, 4)}")

  // 5.
  object Reverse extends App {
    println(args.reverse.mkString(" "))
  }

  // 6, 7. 
  object Card extends Enumeration {
    type Card = Value
    val spade = Value("♠")
    val heart = Value("♥")
    val clubs = Value("♣")
    val diamond = Value("♦")
    import Card._
    def isRed(card: Card) = (card == diamond || card == heart)
  }

  println(s"card ${Card.spade} is red? ${Card.isRed(Card.spade)}")

  //8. Write an enumeration describing the eight corners of the RGB color cube. As
  //IDs, use the color values (for example, 0xff0000 for Red).

  object RGBCube extends Enumeration {
    val Black = Value(0x000000, "Black")
    val Red = Value(0xff0000, "Red")
    val Green = Value(0x00ff00, "Green")
    val Yellow = Value(0xffff00, "Yellow")
    val Blue = Value(0x0000ff, "Blue")
    val Cyan = Value(0x00ffff, "Cyan")
    val Magenta = Value(0xff00ff, "Magenta")
    val White = Value(0xffffff, "White")
  }
}
