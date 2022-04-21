import scala.collection.mutable.ArrayBuffer

class Solution (val openFacilities: Array[Boolean]) {

  def facilities : Array[Int] = {
    val open = ArrayBuffer[Int]()
    for (i <- openFacilities.indices) {
      if (openFacilities(i))
        open += (i+1)
    }
    open.toArray
  }

  override def toString: String = {
    val open = facilities
    s"Open facilities are ${open.mkString("(", ", ", ")")}"
  }

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
  val sol = Array[Boolean](true,false,true,false)
  val sol2 = Solution(sol)
  //println(sol2.eval(inst2))
  println(sol2)
}


