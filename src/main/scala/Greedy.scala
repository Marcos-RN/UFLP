import util.Logger

/* Implementation of Greedy Algorithm for solving ULFP.
   instance is the problem to be solved.
   */

class Greedy (val instance: Instance) {

  def solve : Solution = {
    // Current solution (all facilities closed initially)
    val solution = Array.ofDim[Boolean](instance.numLocations)
    val evalSol = Array.ofDim[Double](instance.numLocations)
    // Array including the best options for each facility, it will be updated in every iteration
    val bestOption = Array.ofDim[Double](instance.numCustomers)
    // Consider all singleton (with just one open facility) solutions
    for (i <- 0 until instance.numLocations) {
      // Compute objective value of tentative solution
      var sum = 0.0
      for (j <- 0 until instance.numCustomers) {
        sum += instance.serviceCost(j)(i)
      }
      sum += instance.openCost(i)
      evalSol(i) = sum
    }
    // Chose the best singleton solution
    val first = evalSol.min
    val ind = evalSol.indexOf(first)
    solution(ind) = true
    // Update the bestOption array with the only open facility costs
    for (i <- bestOption.indices) {
      bestOption(i) = instance.serviceCost(i)(ind)
    }
    // Define a sentinel value as a condition of termination (if false , then stop)
    var goOn = true
    var solValue = first
    while (goOn){
      var newFacVal = solValue
      var newFacInd = 0
      // Try to open one of yet closed facilities
      for (i <- solution.indices) {
        if (!solution(i)) {
          // Compute new objective value
          var objValue = solValue
          objValue += instance.openCost(i)
          for (j <- bestOption.indices) {
            if (instance.serviceCost(j)(i) < bestOption(j))
              objValue -= (bestOption(j)-instance.serviceCost(j)(i))
          }
          // If it improves current solution, make it a tentative new solution
          if (objValue < newFacVal) {
            newFacVal = objValue
            newFacInd = i
          }
        }
      }
      // If the objective value of the best solution computed above is better than the current one,
      // open the facility that makes this value possible.
      if (newFacVal < solValue) {
        // Update bestOptions and current solution for representing new current solution
        solution(newFacInd) = true
        for (i <- bestOption.indices) {
          if (instance.serviceCost(i)(newFacInd) < bestOption(i))
            bestOption(i) = instance.serviceCost(i)(newFacInd)
        }
        solValue = newFacVal
      }
      // If no better value is possible using this procedure, while loop ends.
      else goOn = false
    }
    Solution.apply(solution, solValue)
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
  //val sol = instGreedy.solve
  //println(sol.eval(inst))
  val inst = Instance.fromFileOrLib("cap74.txt")
  val inst2 = Instance.random(0, 500, 150)
  val instGreedy = Greedy(inst)
  val instGreedy2 = Greedy(inst2)
  val sol = instGreedy.solve
  //val sol2 = instGreedy2.solve
  println(sol)
}


object MainGreedy extends App {
  // Use English formats
  import java.util.Locale
  Locale.setDefault(Locale.ENGLISH)

  if(args.length < 1) {
    println("Usage: <file>")
    System.exit(0)
  }

  val fileName = args(0)

  val instance =
    if(fileName.contains("ORlib"))
      Instance.fromFileOrLib(fileName)
    else if(fileName.contains("Simple"))
      Instance.fromFileSimple(fileName)
    else
      Instance.fromFile(fileName)

  println(s"Running Greedy on $fileName.")

  val logger: Logger[Double] = Logger[Double]()
  val greedy = Greedy(instance)
  val solution = greedy.solve
  logger.register("iter: %20d   fitness: %20.8f   time: %20.8f", 0, solution.objectiveValue)
  println(solution)
  logger.print()
}
