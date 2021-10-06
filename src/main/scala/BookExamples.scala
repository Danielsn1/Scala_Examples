class BankAccount(initialBalance: Double){
  private var balance = initialBalance
  def currentBalance = balance
  def deposit(amount:Double) = {balance += amount; balance}
  def withdraw(amount:Double) = {balance -= amount; balance}
}

class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance){
  override def deposit(amount: Double):Double = super.deposit(amount - 1.0)
  override def withdraw(amount: Double): Double = super.withdraw(amount + 1.0)
}

class SavingsAccount(initialBalance: Double, private val interestRate:Double = 0.0004) extends BankAccount(initialBalance){
  private var transactions = 0
  override def deposit(amount: Double): Double = if(transactions < 3){transactions += 1;super.deposit(amount)}
  else{transactions += 1 ; super.deposit(amount - 1)}
  override def withdraw(amount: Double): Double = if(transactions < 3){transactions += 1;super.withdraw(amount)}
  else{transactions += 1 ; super.withdraw(amount + 1)}
  def earnMonthlyInterest(): Double ={ transactions = 0 ; super.deposit(super.currentBalance * (interestRate/12))}
}

case class Point(x:Double, y:Double)

class LabeledPoint(label:String,override val x:Double,override val y:Double) extends Point(x,y)

abstract class Shape(){
  def centerPoint(): LabeledPoint

}

class Rectangle(p1:Point, p2:Point) extends Shape{
  override def centerPoint(): LabeledPoint = new LabeledPoint("Center Point",(p2.x + p1.x)/2.0,(p2.y + p1.y)/2.0)
}

class Circle(center:Point, radius:Double) extends Shape{
  override def centerPoint(): LabeledPoint = new LabeledPoint("Center Point",center.x,center.y)
}

object BookExamples extends  App {
  val savings = new SavingsAccount(100)
  for(i <- 0 until 10){
    println(savings.deposit(10))
  }
  println(savings.earnMonthlyInterest())
  println(savings.deposit(10))
}
