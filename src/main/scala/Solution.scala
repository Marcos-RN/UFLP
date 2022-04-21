import scala.collection.mutable.ArrayBuffer

class Solution (val openFacilities: Array[Boolean]) {

  //def facilities :

  def eval(instance: Instance): Double = {
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
    for (h <- openFacilities.indices) {
      if (openFacilities(h)) total += instance.openCost(h)
    }
    total
  }
}


object Solution {

  def apply(sol : Array[Boolean]): Solution = {
    new Solution(sol)
  }
}

object solTest extends App {
  val inst2 = Instance.fromFile("instejemplo2.txt")
  val sol = Array[Boolean](true,true,true,true)
  val sol2 = Solution(sol)
  println(sol2.eval(inst2))
}


