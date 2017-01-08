package object random {
  var Array(next, a, b, n) = Array(0, 1664525, 1013904223, 32)
  def nextInt(): Int = { 
    next = (next * a + b) % 2*n 
    next
  }
  def nextDouble(): Double = {
    next = (next * a + b) % 2*n
    next.toDouble
  }
  def setSeed(seed: Int) {
    next = seed
  }
}
package random {}

