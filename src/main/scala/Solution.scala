import scala.collection.mutable.ArrayBuffer

class Solution (val openFacilities: Array[Boolean]) {

  //def facilities :

  def eval(instance: Instance) : Double = {
      var total = 0.0
      for (i <- 0 until instance.numCustomers) {
        val customer = instance.serviceCost(i)
        val open = ArrayBuffer[Double]()
        for (k <- customer.indices) {
          if (openFacilities(k)) open += customer(k)
        }
        val chosenFacility = open.min
        total += chosenFacility
      }
      for (h <- openFacilities.indices) {
        if (openFacilities(h)) total += instance.openCost(h)
      }
      total
    }
  }


object Solution extends App {

  def apply(sol : Array[Boolean]): Solution = {
    new Solution(sol)
  }
}

object solTest extends App {
  val inst2 = Instance.fromFile("instejemplo.txt")
  val sol = Array[Boolean](true,false,false,false)
  val sol2 = Solution(sol)
  println(sol2.eval(inst2))
}


