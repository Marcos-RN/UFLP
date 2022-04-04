class Solution (val sol: Array[Boolean], val service : Array[Array[Int]]) {

}

object Solution extends App {
  def eval(instance: Instance, solution: Solution): Double = {
    val openFacilities = solution.sol
    val serv = solution.service
    val openCost = instance.openCost
    val serviceCost = instance.serviceCost
    var total = 0.0
    for (i <- 0 until instance.numLocations) {
      if (openFacilities(i)) total += openCost(i)
      val values = serv(i)
      for (j <- values.indices) {
        total += serviceCost(i)(j)
      }
    }
    total
  }
}


