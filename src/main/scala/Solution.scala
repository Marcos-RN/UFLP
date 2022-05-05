import scala.collection.mutable.ArrayBuffer

class Solution(val openFacilities: Array[Boolean], val objectiveValue: Double) {
  def facilities: Array[Int] = {
    val open = ArrayBuffer[Int]()
    for (i <- openFacilities.indices)
      if (openFacilities(i))
        open += i
    open.toArray
  }

  override def toString: String =
    s"Open facilities are ${facilities.mkString("(", ", ", ")")}\nObjective value is $objectiveValue"
}

object Solution {
  def apply(openFacilities: Array[Boolean], objectiveValue: Double): Solution =
    new Solution(openFacilities, objectiveValue)

  def apply(openFacilities: Array[Boolean], instance: Instance): Solution =
    new Solution(openFacilities, eval(openFacilities, instance))

  def eval(openFacilities: Array[Boolean], instance: Instance): Double = {
    var total = 0.0
    for (i <- 0 until instance.numCustomers) {
      val customer = instance.serviceCost(i)
      var minimum = Double.MaxValue
      for (k <- customer.indices) {
        if (openFacilities(k))
          if (customer(k) < minimum)
            minimum = customer(k)
      }
      total += minimum
    }
    for (h <- openFacilities.indices)
      if (openFacilities(h))
        total += instance.openCost(h)
    total
  }
}

object solTest extends App {
  val instance = Instance.fromFile("instejemplo2.txt")
  val openFacilities = Array[Boolean](true, false, true, false)
  val solution = Solution(openFacilities, Solution.eval(openFacilities, instance))
  println(solution)
}


