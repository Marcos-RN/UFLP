import scala.collection.mutable.ArrayBuffer

/* Implementation of Solution class for representing and working with solutions for the UFLP.
   openFacilities is a Boolean Array in which the open facilities are represented by "true"
   and the closed ones by "false"; objectiveValue is the value of the objective function
   given the open facilities
   */

class Solution(val openFacilities: Array[Boolean], val objectiveValue: Double) {

  //Given a solution, it returns an array with the indexes of the open facilities
  def facilities: Array[Int] = {
    val open = ArrayBuffer[Int]()
    for (i <- openFacilities.indices)
      if (openFacilities(i))
        open += i
    open.toArray
  }

  //This line allows to print a solution
  override def toString: String =
    s"Open facilities are ${facilities.mkString("(", ", ", ")")}\nObjective value is $objectiveValue"
}

object Solution {
  def apply(openFacilities: Array[Boolean], objectiveValue: Double): Solution =
    new Solution(openFacilities, objectiveValue)

  def apply(openFacilities: Array[Boolean], instance: Instance): Solution =
    new Solution(openFacilities, eval(openFacilities, instance))

  //Evaluate the solution given an instance, it returns the objective value
  def eval(openFacilities: Array[Boolean], instance: Instance): Double = {
    var total = 0.0
    //For every customer, search among the open facilities which one gives the minimum service cost
    //and sums this values
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
    //Include the open cost for the open facilities
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


