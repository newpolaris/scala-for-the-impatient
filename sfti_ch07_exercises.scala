/* sfti_ch07_ex01.scala

package com {
  package object horstmann {
    def hello() = println("HI")
  }
  package horstmann {
    package object impatient {
      def hi() = hello()        
    }
  }
}

package com.horstmann.impatient {
  object Obj {
    // hello() // compile error 
    com.horstmann.hello() // OK
  }
}
*/
/* sfti_ch07_ex03.scala

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
*/

object sfti extends App {
  // 2. skip
  // 3. Write a package random with functions nextInt(): Int, nextDouble(): Double, and
  // setSeed(seed: Int): Unit. To generate random numbers, use the linear
  // congruential generator
  // next = previous * a + b mod 2n,
  // where a = 1664525, b = 1013904223, and n = 32.
  random.setSeed(10)
  println(s"random.nextInt ${random.nextInt}")

  // 6, 7  
  import java.util.{ HashMap => JavaMap }
  var javaMap = new JavaMap[String, String]
  javaMap.put("1", "a")
  javaMap.put("2", "b")
  javaMap.put("3", "c")

  import scala.collection.JavaConverters._
  var scalaMap = javaMap.asScala
  println(s"scalaMap $scalaMap")

  // 9.
  import java.lang.System._
  var username = getProperty("user.name")
  var password = readLine()

  if (password == "secret")
    println("CORRECT")
  else {
    // impossible;
    err.println("WRONG")
  }
}
