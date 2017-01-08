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
