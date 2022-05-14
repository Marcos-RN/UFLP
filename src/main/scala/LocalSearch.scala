
/*
class LocalSearch(val instance: Instance, val solution: Solution) {

  def neighbourSearch(bestOption : Array[Double]) : Solution = {
    var goOn = true
    var i = 0
    var objValue = solution.objectiveValue
    val openFac = solution.openFacilities
    while (goOn) {
      for (i <- solution.openFacilities.indices) {
        openFac(i) = !solution.openFacilities(i)
        val objValueN = Solution.eval(solution.openFacilities, instance)
        if (objValueN < objValue)
          goOn = true
      }

    }
    }

  def HillClimbing : Solution = {
    val bestOption = Array.ofDim[Double](instance.numCustomers)
    for (i <- bestOption.indices) {
      bestOption(i) = instance.serviceCost(i)(0)
      for (j <- 1 until instance.numLocations) {
        if (instance.serviceCost(i)(j) < bestOption(i))
          bestOption(i) = instance.serviceCost(i)(j)
      }
    }
    var objValue = solution.objectiveValue
    var goOn = true
    while (goOn) {
      for (i <- solution.openFacilities.indices) {

      }
    }




    solution
  }
}
*/