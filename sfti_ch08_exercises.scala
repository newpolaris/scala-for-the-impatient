object sfti extends App {
  // 1.
  class BankAccount(initialBalance: Double) {
    protected var balance = initialBalance
    def deposit(amount: Double) = { balance += amount; balance }
    def withdraw(amount: Double) = { balance -= amount; balance }
  }

  class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
    private val comission = 1.0
    override def deposit(amount: Double) = {
      balance -= comission;
      super.deposit(amount)
    }
    override def withdraw(amount: Double) = {
      balance -= comission;
      super.withdraw(amount)
    }
  }

  // 2.
  class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance) {
    private val comission = 1.0
    private val interestRate = 0.04
    private val freeTransactions = 3
    private var transaction = 0
    def earnMonthlyInterest() {
      transaction = 0
      balance *= (1.0 + interestRate)
    }
    override def deposit(amount: Double) = {
      if (transaction >= freeTransactions) 
        balance -= comission
      super.deposit(amount)
      transaction += 1
      balance
    }

    override def withdraw(amount: Double) = {
      if (transaction >= freeTransactions) 
        balance -= comission
      super.withdraw(amount)
      transaction += 1
      balance
    }
  }

  // 3. Skip
  // 4. 
  abstract class Item {
    def price:Double
    def description:String
    override def toString() = { f"${price}%.1f$$ ${description}" }
  }
  class SimpleItem(override val price:Double, override val description:String) extends(Item)
  class Bundle(initItems: Item*) {
    private var items = initItems.toBuffer
    def append(item: Item) { items += item }
    def description() = s"[ ${items.mkString(", ")} ]"
  }

  var bundle = new Bundle(new SimpleItem(1, "Moby Dick"), new SimpleItem(2, "Jurassic Park")) 
  bundle.append(new SimpleItem(3, "Peter Pan"))
  println(s"bundle ${bundle.description}")

  // 5.
  class Point(var x: Double, var y: Double) {
    def +(that: Point):Point = new Point(that.x+x, that.y+y)
    def /(div: Double):Point = new Point(x/div, y/div)
  }
  class LabeledPoint(var Label:String, x: Double, y: Double) extends Point(x, y) {
    override def toString() = s"$x $y $Label"
  }
  println(s"Labed Point: ${new LabeledPoint("Black Thursday", 1929, 230.07)}")

  // 6.
  abstract class Shape() {
    def centerPoint:Point
  }
  class Rectangle(var lt: Point, var rb: Point) extends Shape {
    override def centerPoint = (lt+rb)/2
  }
  class Circle(override val centerPoint: Point, var radius: Double) extends Shape

  // 7.
  class Square(x: Int, y:Int, width:Int, height:Int) extends java.awt.Rectangle(x, y, width, height) {
    def this() = this(0, 0, 0, 0)
    def this(width:Int) = this(0, 0, width, width)
    def this(x:Int, y:Int, width:Int) = this(x, y, width, width)
  }

  // 8.
  class Person(val name: String) {
    override def toString = getClass.getName + "[name=" + name + "]"
  }

  class SecretAgent(codename: String) extends Person(codename) {
    override val name = "secret"
    override val toString = "secret"
  }
  /*
    Compiled from "sfti_ch08_exercises.scala"
    public class sfti$Person {
      private final java.lang.String name;

      public java.lang.String name();
        Code:
           0: aload_0
           1: getfield      #13                 // Field name:Ljava/lang/String;
           4: areturn

      public java.lang.String toString();
        Code:
           0: new           #18                 // class java/lang/StringBuilder
           3: dup
           4: invokespecial #22                 // Method java/lang/StringBuilder."<init>":()V
           7: aload_0
           8: invokevirtual #26                 // Method getClass:()Ljava/lang/Class;
          11: invokevirtual #31                 // Method java/lang/Class.getName:()Ljava/lang/String;
          14: invokevirtual #35                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
          17: ldc           #37                 // String [name=
          19: invokevirtual #35                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
          22: aload_0
          23: invokevirtual #39                 // Method name:()Ljava/lang/String;
          26: invokevirtual #35                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
          29: ldc           #41                 // String ]
          31: invokevirtual #35                 // Method java/lang/StringBuilder.append:(Ljava/lang/String;)Ljava/lang/StringBuilder;
          34: invokevirtual #43                 // Method java/lang/StringBuilder.toString:()Ljava/lang/String;
          37: areturn

      public sfti$Person(java.lang.String);
        Code:
           0: aload_0
           1: aload_1
           2: putfield      #13                 // Field name:Ljava/lang/String;
           5: aload_0
           6: invokespecial #45                 // Method java/lang/Object."<init>":()V
           9: return
    }
    Compiled from "sfti_ch08_exercises.scala"
    public class sfti$SecretAgent extends sfti$Person {
      private final java.lang.String name;

      private final java.lang.String toString;

      public java.lang.String name();
        Code:
           0: aload_0
           1: getfield      #15                 // Field name:Ljava/lang/String;
           4: areturn

      public java.lang.String toString();
        Code:
           0: aload_0
           1: getfield      #19                 // Field toString:Ljava/lang/String;
           4: areturn

      public sfti$SecretAgent(java.lang.String);
        Code:
           0: aload_0
           1: aload_1
           2: invokespecial #24                 // Method sfti$Person."<init>":(Ljava/lang/String;)V
           5: aload_0
           6: ldc           #26                 // String secret
           8: putfield      #15                 // Field name:Ljava/lang/String;
          11: aload_0
          12: ldc           #26                 // String secret
          14: putfield      #19                 // Field toString:Ljava/lang/String;
          17: return
    }
  */
  // 9.
  class Creature {
    def range: Int = 10
    val env: Array[Int] = new Array[Int](range)
  }

  class Ant extends Creature {
    override def range = 2
  }
  var ant = new Ant
  println(s"env length ${ant.env.length}")
}
