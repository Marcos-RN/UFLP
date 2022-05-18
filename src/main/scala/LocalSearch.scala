

/*class LocalSearch(val instance: Instance, val solution: Solution) {

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
      bestOption(i) = Double.MaxValue
      for (j <- 0 until instance.numLocations) {
        if (solution.openFacilities(j)) {
          if (instance.serviceCost(i)(j) < bestOption(i)) {
            bestOption(i) = instance.serviceCost(i)(j)
          }
        }
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
}*/

class LocalSearch(val instance: Instance, val solution: Solution) {

  def changeLoc(i : Int) : Solution = {
    val solnew = Solution.apply(solution.openFacilities, instance)
    solnew.openFacilities(i) = !solution.openFacilities(i)
    val objVal = Solution.eval(solnew.openFacilities, instance)
    Solution.apply(solnew.openFacilities, objVal)
  }


  def HillClimbing : Solution = {
    var objValue = solution.objectiveValue
    var openFac = solution.openFacilities
    var goOn = true
    var c = 0
    while (goOn && c < instance.numLocations) {
      for (i <- solution.openFacilities.indices) {
        if (changeLoc(i).objectiveValue < objValue) {
          openFac = changeLoc(i).openFacilities
          objValue = changeLoc(i).objectiveValue
          goOn = false
        }
        c += 1
      }
    }
    Solution.apply(openFac,objValue)
  }


}