import scala.::
import scala.collection.mutable.ListBuffer

class Triple(val a:Int, val b:Int, val c:Int){
  override def toString: String = f"$a%4d, $b%4d, $c%4d"
}

object MathWorld extends App {
  val sequence: List[Int] = List.range(2,15)
  var list = new ListBuffer[Int]

  list.addOne(1)

  for(n <- sequence) println(f"sample = $n%4d")

  def pythagorean(n:Int) = {
    val a = 2 * n
    val b = n * n - 1
    val c = n * n + 1
    new Triple(a,b,c)
  }

  val pythagoreanTriples =
    (for (n <- sequence) yield pythagorean(n))

  println

  for(t <- pythagoreanTriples) println(f"triple = $t")
}

