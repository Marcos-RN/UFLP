import util.Logger
import util.random.xoShiRo256StarStar.Random
import scala.collection.mutable

/* Implementation of Greedy Randomized Adaptive Search Procedure
   for solving ULFP. instance is the problem to be solved and
   rnd is source of randomness for implementing an stochastic
   algorithm
 */
class GRASP(val instance: Instance, val rnd: Random) {

  /* Items stored in priority queue represent new solutions obtained
     from current one by opening a single new facility. They consist
     of index of new open facility and objective value of new solution.
     An order relationship is defined for the priority queue so that
     items with smaller objective values have higher priorities.
  */
  case class PQItem(index: Int, value: Double) extends Ordered[PQItem] {
    def compare(that: PQItem): Int = -this.value.compare(that.value)
  }


  // RCLsz parameter defines size of restricted candidate list and
  // inversely defines greedyness of algorithm
  def solve(RCLsz: Int): Solution = {
    // Current solution (all facilities closed initially)
    val solution = Array.ofDim[Boolean](instance.numLocations)

    // Priority queue of new tentative solutions
    val pq = new mutable.PriorityQueue[PQItem]()

    // Consider all singleton (with just one open facility) solutions
    for (i <- 0 until instance.numLocations) {
      // Compute objective value of tentative solution
      var objValue = instance.openCost(i)
      for (j <- 0 until instance.numCustomers)
        objValue += instance.serviceCost(j)(i)

      // Add tentative solution to priority queue
      pq.enqueue(PQItem(i, objValue))
    }

    // Chose randomly one of best RCLsz tentative solutions
    val chosen = rnd.uniform(RCLsz min pq.size)
    var first: PQItem = null
    for (i <- 0 to chosen)
      first = pq.dequeue()

    // Update current solution and its value
    solution(first.index) = true
    var solValue = first.value

    // update bestOptions corresponding to current solution
    val bestOption = Array.ofDim[Double](instance.numCustomers)
    for (i <- bestOption.indices)
      bestOption(i) = instance.serviceCost(i)(first.index)

    var goOn = true
    while (goOn) {
      // Reset priority queue for this iteration
      pq.clear()

      // Try to open one of yet closed facilities
      for (i <- solution.indices) {
        if (!solution(i)) {
          // Compute new objective value
          var objValue = solValue
          objValue += instance.openCost(i)
          for (j <- bestOption.indices) {
            if (instance.serviceCost(j)(i) < bestOption(j))
              objValue -= (bestOption(j) - instance.serviceCost(j)(i))
          }
          // If it improves current solution, make it a tentative new solution
          if (objValue < solValue)
            pq.enqueue(PQItem(i, objValue))
        }
      }

      if (pq.isEmpty)
        goOn = false // No improving tentative solutions
      else {
        // Chose randomly one of best RCLsz improving solutions
        val chosen = rnd.uniform(RCLsz min pq.size)
        var pqSel: PQItem = null
        for (k <- 0 to chosen)
          pqSel = pq.dequeue()

        // Update current solution and its value
        val ind = pqSel.index
        solution(ind) = true
        solValue = pqSel.value

        // Update bestOptions for representing new current solution
        for (i <- bestOption.indices)
          if (instance.serviceCost(i)(ind) < bestOption(i))
            bestOption(i) = instance.serviceCost(i)(ind)
      }
    }
    Solution.apply(solution, solValue)
  }
}

object GRASP {
  def apply(instance: Instance, rnd: Random): GRASP = {
    new GRASP(instance, rnd)
  }
}

class IteratedGRASP(val instance: Instance, val rnd: Random, RCLsz: Int) {
  // Crea un logger, que permite registrar soluciones de tipo Double y que incluye un temporizador
  val logger: Logger[Double] = Logger[Double]()

  def solve(maxTime: Double): Solution = {
    var iter = 0
    var bestSolution: Solution = null

    val grasp = GRASP(instance, rnd)
    // Ejecutar hasta maxTime segundos
    while(logger.timer.elapsedTime() < maxTime) {
      val sol = grasp.solve(RCLsz)

      // Si la solución mejora, la registramos en el logger, junto con la iteración.
      // El logger guarda automáticamente el tiempo
      if(bestSolution == null || sol.objectiveValue < bestSolution.objectiveValue) {
        bestSolution = sol
        logger.register("iter: %20d   fitness: %20.8f   time: %20.8f", iter, bestSolution.objectiveValue)
      }
      iter += 1
    }
    bestSolution
  }
}

object IteratedGRASP {
  def apply(instance: Instance, rnd: Random, RCLsz: Int): IteratedGRASP =
    new IteratedGRASP(instance, rnd, RCLsz)
}

object IteratedGRASPTest extends App {
  // Use English formats
  import java.util.Locale
  Locale.setDefault(Locale.ENGLISH)

  def test(instance: Instance, seed: Int): Unit = {
    val RCLsz = 8
    val maxTime = 60 // seconds
    val rnd = Random(seed)
    val grasp = IteratedGRASPLS(instance, rnd, RCLsz)
    val solution = grasp.solve(maxTime)
    println(solution)
    grasp.logger.print()
  }
  test(ExampleInstances.inst2, 0)
}

object MainIteratedGRASP extends App {
  // Use English formats
  import java.util.Locale
  Locale.setDefault(Locale.ENGLISH)

  if(args.length < 4) {
    println("Usage: <seed> <file> <RCLsz> <maxTime>")
    System.exit(0)
  }

  val seed = args(0).toInt
  val fileName = args(1)
  val RCLsz = args(2).toInt
  val maxTime = args(3).toDouble

  val rnd = Random(seed)

  val instance =
    if(fileName.contains("ORlib"))
      Instance.fromFileOrLib(fileName)
    else if(fileName.contains("Simple"))
      Instance.fromFileSimple(fileName)
    else
      Instance.fromFile(fileName)

  println(s"Running IteratedGRASP on $fileName. seed=$seed, RCLsz=$RCLsz, maxTime=$maxTime")

  val grasp = IteratedGRASP(instance, rnd, RCLsz)
  val solution = grasp.solve(maxTime)
  println(solution)
  grasp.logger.print()
}



class IteratedGRASPLS(val instance: Instance, val rnd: Random, RCLsz: Int) {
  // Crea un logger, que permite registrar soluciones de tipo Double y que incluye un temporizador
  val logger: Logger[Double] = Logger[Double]()

  def solve(maxTime: Double): Solution = {
    var iter = 0
    var bestSolution: Solution = null

    val grasp = GRASP(instance, rnd)
    // Ejecutar hasta maxTime segundos
    while(logger.timer.elapsedTime() < maxTime) {
      val sol = grasp.solve(RCLsz)

      val localSearch = LocalSearch(instance, sol)
      val localSearchSol = localSearch.HillClimbing


      // Si la solución mejora, la registramos en el logger, junto con la iteración.
      // El logger guarda automáticamente el tiempo
      if(bestSolution == null || localSearchSol.objectiveValue < bestSolution.objectiveValue) {
        bestSolution = localSearchSol
        logger.register("iter: %20d   fitness: %20.8f   time: %20.8f", iter, bestSolution.objectiveValue)
      }
      iter += 1
    }
    bestSolution
  }
}

object IteratedGRASPLS {
  def apply(instance: Instance, rnd: Random, RCLsz: Int): IteratedGRASPLS =
    new IteratedGRASPLS(instance, rnd, RCLsz)
}

object MainIteratedGRASPLS extends App {
  // Use English formats
  import java.util.Locale
  Locale.setDefault(Locale.ENGLISH)

  if(args.length < 4) {
    println("Usage: <seed> <file> <RCLsz> <maxTime>")
    System.exit(0)
  }

  val seed = args(0).toInt
  val fileName = args(1)
  val RCLsz = args(2).toInt
  val maxTime = args(3).toDouble

  val rnd = Random(seed)

  val instance =
    if(fileName.contains("ORlib"))
      Instance.fromFileOrLib(fileName)
    else if(fileName.contains("Simple"))
      Instance.fromFileSimple(fileName)
    else
      Instance.fromFile(fileName)

  println(s"Running IteratedGRASPLS on $fileName. seed=$seed, RCLsz=$RCLsz, maxTime=$maxTime")

  val grasp = IteratedGRASPLS(instance, rnd, RCLsz)
  val solution = grasp.solve(maxTime)
  println(solution)
  grasp.logger.print()
}

