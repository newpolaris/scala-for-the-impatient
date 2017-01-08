object sfti extends App {
  // ex2.1
  val n = 10
  val sol1 = Seq.fill(n)(scala.util.Random.nextInt(n))
  println(s"ex1 : $sol1")
  // 6. Write a for loop for computing the product of the Unicode codes of all letters
  // in a string. For example, the product of the characters in "Hello" is 9415087488.
  var prod = 1L                              //> prod  : Int = 1
  for(c <- "Hello") prod *= c
  prod                                      //> res3: Int = 9415087488

  // 7. Solve the preceding exercise without writing a loop. (Hint: Look at the StringOps
  // Scaladoc.)
  "Hello".foldLeft(1L)((a, b) => a * b)      //> res4: Int = 9415087488

  // 8. Write a function product(s : String) that computes the product, as described
  // in the preceding exercises.
  def product(s: String) = s.foldLeft(1L)((a, b) => a * b)
  //> product: (s: String)Int
  product("Hello")                                //> res5: Int = 9415087488

  // 9. Make the function of the preceding exercise a recursive function.
  def productRec(s: String):Long = {
    if(s.length == 0) 1
    else s(0) * productRec(s drop 1)
  }                                         //> productRec: (s: String)Int
  productRec("Hello")                       //> res6: Int = 9415087488
  // ex2.10
  def pow(x: Int, n:Int):Double = {
    if (n > 0) {
      if (n % 2 == 0) {
        val xn = pow(x, n/2)
        xn*xn
      }
      else x*pow(x, n-1)
    } 
    else if (n == 0) 1 
    else 1/pow(x, -n)
  }
  println(pow(2, 10))
  println(pow(2, -10))
}
