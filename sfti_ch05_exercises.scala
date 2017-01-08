object sfti extends App {
  // 1. Improve the Counter class in Section 5.1, "Simple Classes and Parameterless
  // Methods," on page 51 so that it doesn't turn negative at Int.MaxValue.
  class Counter {
    private var value = 0
    def increment() { 
      if (Int.MaxValue > value)
        value += 1 
    }
    def current() = value
  }

  // 2. Write a class BankAccount with methods deposit and withdraw, and a read-only
  // property balance.
  class BankAccount() {
    private var _balance = 0
    def balance = _balance
    def deposit(money: Int) {
      _balance += money
    }
    def withdraw(money: Int) {
      if (balance - money >= 0)
        _balance -= money
    }
  }

  var acc = new BankAccount
  println(s"blanace ${acc.balance}")
  acc.deposit(10)
  println(s"new blanace ${acc.balance}")

  // 3. Write a class Time with read-only properties hours and minutes and a method
  // before(other: Time): Boolean that checks whether this time comes before the
  // other. A Time object should be constructed as new Time(hrs, min), where hrs is in
  // military time format (between 0 and 23).
  class Time(val hours:Int, val minutes:Int) {
    def before(other: Time) = {
      (other.hours > hours) || (other.hours == hours && other.minutes > minutes)
    }
  }
  var time = new Time(12, 30)
  println(s"hours:minutes ${time.hours}:${time.minutes}")
  var time_before = new Time(12, 29)
  println(s"is time before ${time_before.before(time)}")

  // 4.
  class Time2(h:Int, m:Int) {
    private val pass = (h*60 + m) % (24*60 - 1)
    def hours:Int = pass / 60
    def minutes:Int = pass % 60
    def before(other: Time2): Boolean = pass < other.pass
  }
  var time2 = new Time2(12, 30)
  println(s"hours:minutes ${time2.hours}:${time2.minutes}")
  var time_before2 = new Time2(12, 29)
  println(s"is time before ${time_before2.before(time2)}")

  // 5.
  import scala.beans.BeanProperty

  class Student (@BeanProperty var id:Long, @BeanProperty var name:String) {}
  /*
    Compiled from "sfti_ch05_exercises.scala"
    public class sfti$Student {
    public long id();
    public void id_$eq(long);
    public java.lang.String name();
    public void name_$eq(java.lang.String);
    public long getId();
    public void setId(long);
    public java.lang.String getName();
    public void setName(java.lang.String);
    public sfti$Student(long, java.lang.String);
  */

  var student = new Student(0, "James")
  println(s"name : ${student.getName()}")
  student.name = "Hello"
  println(s"name : ${student.getName()}")

  // 6.
  class Person(var age:Int) {
    if (age < 0) age = 0
  }

  // 7.
  class Person2(name:String) {
    val Array(firstName, lastName) = name.split(' ')
  }

  val p = new Person2("Fred Smith")
  println(s"first last name: ${p.firstName + ' ' + p.lastName}")

  // 8. 
  class Car(val manufac:String, val model:String, val year:Int, val licensePlate:String) {
    def this(manufac: String, model:String, licensePlate:String) {
      this(manufac, model, -1, licensePlate)
    }
    def this(manufac:String, model:String, year:Int) {
      this(manufac, model, year, "")
    }
    def this(manufac:String, model:String) {
      this(manufac, model, -1, "")
    }
    override def toString = {
      manufac + " " + model + " " + year + " " + licensePlate
    }
  }

  // 9.
  /*
    class Car {
    public:
      Car(string manufac, string model, string licensePlate, int year = -1) : 
        _maker(manufac), _model(model), _num(licensePlate), _year(year) {}
      Car(string manufac, string model, string licensePlate, int year = -1) : 
        _maker(manufac), _model(model), _num(licensePlate), _year(year) {}
      string& licensePlate() { return _num; }
    private:
      string _maker, _model, _num
      int _year
    };
  */
  // 10.
  class Employee(val name: String = "John Q. Public", val salary: Double = 0.0) {}

}
