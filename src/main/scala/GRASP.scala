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

object graspTest extends App {
  java.util.Locale.setDefault(java.util.Locale.ENGLISH)
  val inst2 = Instance.fromFileOrLib("cap74.txt")
  val inst = Instance.random(0, 500, 150)
  val rnd = new Random(0)
  val instGrasp = GRASP(inst, rnd)
  val instGrasp2 = GRASP(inst2, rnd)
  var bestValue = Double.MaxValue
  val maxIterations = 100
  for(i <- 0 until maxIterations) {
    val sol2 = instGrasp2.solve(10)
    if(sol2.objectiveValue < bestValue) {
      bestValue = sol2.objectiveValue
      println(sol2)
    }
  }
}

object LoggerExample extends App {
  // Use English formats
  import java.util.Locale
  Locale.setDefault(new  Locale.Builder().setLanguage("en").setRegion("US").build())

  java.util.Locale.setDefault(java.util.Locale.ENGLISH)
  val inst = Instance.random(0, 500, 150)
  val inst2 = Instance.fromFileOrLib("cap74.txt")
  val rnd = new Random(0)
  val instGrasp = GRASP(inst, rnd)
  val instGrasp2 = GRASP(inst2, rnd)



  // Crea un logger, que permite registrar soluciones de tipo Double y que incluye un temporizador
  val logger = util.Logger[Double]()

  var iter = 0
  var best = Double.MaxValue

  // Ejecutar hasta 10 segundos
  while(logger.timer.elapsedTime() < 10.0) {
    val sol = instGrasp.solve(10)
    val fitness = sol.objectiveValue

    // Si la solución mejora, la registramos en el logger, junto con la iteración.
    // El logger guarda automáticamente el tiempo
    if(fitness < best) {
      best = fitness
      logger.register("iter: %20d   fitness: %20.8f   time: %20.8f", iter, fitness)
    }
    iter += 1
  }

  // Mostramos la traza de soluciones registradas en el logger
  logger.print()

}