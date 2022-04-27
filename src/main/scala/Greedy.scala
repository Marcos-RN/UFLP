
class Greedy (val instance: Instance) {

  def solve : Solution = {
    val solution = Array.ofDim[Boolean](instance.numLocations)
    val evalSol = Array.ofDim[Double](instance.numLocations)
    val bestOption = Array.ofDim[Double](instance.numCustomers)
    for (i <- 0 until instance.numLocations) {
      var sum = 0.0
      for (j <- 0 until instance.numCustomers) {
        sum += instance.serviceCost(j)(i)
      }
      sum += instance.openCost(i)
      evalSol(i) = sum
    }
    val first = evalSol.min
    println(first)
    val ind = evalSol.indexOf(first)
    solution(ind) = true
    println(solution.mkString(","))
    for (i <- bestOption.indices) {
      bestOption(i) = instance.serviceCost(i)(ind)
    }
    var goOn = true
    var solValue = first
    while (goOn){
      var newFacVal = solValue
      var newFacInd = 0
      for (i <- solution.indices) {
        if (!solution(i)) {
          var objValue = solValue
          objValue += instance.openCost(i)
          for (j <- bestOption.indices) {
            if (instance.serviceCost(j)(i) < bestOption(j))
              objValue -= (bestOption(j)-instance.serviceCost(j)(i))
          }
          if (objValue < newFacVal) {
            newFacVal = objValue
            newFacInd = i
          }
        }
      }
      if (newFacVal < solValue) {
        solution(newFacInd) = true
        for (i <- bestOption.indices) {
          if (instance.serviceCost(i)(newFacInd) < bestOption(i))
            bestOption(i) = instance.serviceCost(i)(newFacInd)
        }
        solValue = newFacVal
      }
      else goOn = false
      println(solution.mkString(","))
    }
    Solution.apply(solution)
  }
}


object Greedy {

  def apply(instance: Instance): Greedy = {
    new Greedy(instance)
  }
}

object greedyTest extends App {
  java.util.Locale.setDefault(java.util.Locale.ENGLISH)
  //val inst = Instance.fromFile("instejemplo2.txt")
  //val instGreedy = Greedy(inst)
  //val sol = instGreedy.solve
  //println(sol.eval(inst))
  val inst2 = Instance.fromFileOrLib("cap104.txt")
  val instGreedy2 = Greedy(inst2)
  val sol2 = instGreedy2.solve
  println(sol2.eval(inst2))
  println(sol2)
}
