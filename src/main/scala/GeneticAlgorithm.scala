import util.random.xoShiRo256StarStar.Random

/* Implementation of Genetic Algortihm for solving ULFP.
   instance is the problem to be solved
   rnd is source of randomness for implementing an stochastic algorithm
   popSz is the size of the population
   pX is the probability of crossover
   pMut is the probability of mutation
 */

class GeneticAlgorithm(instance: Instance, rnd: Random, popSz: Int, pX: Double, pMut: Double) {

  //The probabilities pX and pMut must be between 0 and 1
  require(0 < pX && pX < 1, "Cross probability must be between 0 and 1")
  require(0 < pMut && pMut < 1, "Mutation probability must be between 0 and 1")

  //Creation of the Individual class, each individual is basically a solution for the problem
  class Individual {
    var solution = Array.fill[Boolean](instance.numLocations)(false)
    //Here will be kept the objective value for each solution
    var fitness: Double = Double.MaxValue

    //This method computes the objective value for the solution
    def computeFitness: Double = Solution.eval(solution, instance)
  }
  //Population represented as an Array of Individuals
  private val population = Array.fill[Individual](popSz)(new Individual)

  //It will be a new child in each iteration of the algorithm
  private val child = new Individual

  def initialization(): Unit = {
    /*Fill each Individual of Population in a random way,
    calculate fitness and sort them in descending order by fitness value
     */
    for (i <- population.indices) {
      for (j <- population(i).solution.indices) {
        val r = rnd.uniform(0.0, 1.0)
        if (r < 0.5)
          population(i).solution(j) = true
      }
      population(i).fitness = population(i).computeFitness
    }
    scala.util.Sorting.quickSort(population)(Ordering.by(sol => -sol.fitness))
  }

  def initializationGRASP(): Unit = {
    /*Fill each Individual of Population using solutions given by GRASP algorithm
      and sort them in descending order by fitness value
     */
    for (i <- population.indices) {
      val rndm = new Random(rnd.nextInt())
      val instGrasp = GRASP(instance, rndm)
      var bestValue = Double.MaxValue
      var mySol = Array[Boolean]()
      val maxIterations = 100
      for (j <- 0 until maxIterations) {
        val sol = instGrasp.solve(10)
        if (sol.objectiveValue < bestValue) {
          bestValue = sol.objectiveValue
          mySol = sol.openFacilities
        }
      }
      population(i).solution = mySol
      population(i).fitness = bestValue
    }
    scala.util.Sorting.quickSort(population)(Ordering.by(sol => -sol.fitness))
  }

  //Returns a "parent" index chosen by binary tournament
  def binaryTournament(): Int = {
    val a = rnd.uniform(popSz)
    val b = rnd.uniform(popSz)
    val index = a min (b)
    index
  }

  //Given two parent indexes, construct a child by uniform crossover
  def uniformCrossover(indexParent1: Int, indexParent2: Int): Unit = {
    val Parent1 = population(indexParent1)
    val Parent2 = population(indexParent2)
    val cut = rnd.uniform(instance.numLocations)
    for (i <- 0 until cut) {
      child.solution(i) = Parent1.solution(i)
    }
    for (i <- cut until instance.numLocations) {
      child.solution(i) = Parent2.solution(i)
    }
    child.fitness = child.computeFitness
  }

  //Given two parent indexes, construct a child by random crossover
  def randomCrossover(indexParent1: Int, indexParent2: Int): Unit = {
    val Parent1 = population(indexParent1)
    val Parent2 = population(indexParent2)
    for (i <- child.solution.indices) {
      val r = rnd.uniform(0.0, 1.0)
      if (r < 0.5)
        child.solution(i) = Parent1.solution(i)
      else
        child.solution(i) = Parent2.solution(i)
    }
    child.fitness = child.computeFitness
  }

  // Copies a random individual from the population to child, just in case crossover is not performed
  def copyToChild(): Unit = {
    val a = rnd.uniform(popSz)
    child.solution = population(a).solution
    child.fitness = population(a).fitness
  }

  // Mutates the "child"
  def mutate(): Unit = {
    for (i <- child.solution.indices) {
      val r = rnd.uniform(0.0, 1.0)
      if (r < pMut)
        child.solution(i) = !child.solution(i)
    }
  }

  // Replaces the worst individual in the population and
  // reorders the population using binary search.
  def replacement(): Unit = {
    var pos = 0
    if (child.fitness < population(0).fitness)
      pos = 0
    else {
      if (child.fitness > population(popSz - 1).fitness)
        pos = popSz - 1
      else {
        var min = 0
        var max = popSz - 2
        var cen = true
        while (cen) {
          if (max - min == 1) {
            pos = max
            cen = false
          }
          else {
            val med = (min + max) / 2
            if (population(med).fitness < child.fitness)
              min = med
            else max = med
          }
        }
      }
    }
    if (pos == popSz - 1) {
      population(popSz - 1) = child
    }
    else {
      for (i <- (pos + 1) until popSz) {
        population(i) = population(i - 1)
      }
      population(pos) = child
    }
  }

  //Genetic Algorithm combining the previous methods
  def solve(maxIter : Int) : Solution = {
    initialization()
    var n = 0
    while (n < maxIter) {
      val r1 = rnd.uniform(0.0, 1.0)
      if (r1 < pX) {
        val p1 = binaryTournament()
        val p2 = binaryTournament()
        uniformCrossover(p1, p2)
      }
      else copyToChild()
      mutate()
      replacement()
      n += 1
    }
    val bestSol = Solution.apply(population(0).solution, population(0).fitness)
    bestSol
  }


}

object GeneticAlgorithm {
  def apply(instance: Instance, rnd: Random, popSz: Int, pX: Double, pMut: Double) : GeneticAlgorithm = {
    new GeneticAlgorithm(instance, rnd, popSz, pX, pMut)
  }
}

object GaTest extends App {
  java.util.Locale.setDefault(java.util.Locale.ENGLISH)
  val inst = Instance.fromFileOrLib("cap74.txt")
  val inst2 = Instance.random(0, 500, 150)
  val rnd = new Random(5)
  val pMut = 1.0/inst.numLocations
  val instGA = GeneticAlgorithm(inst, rnd, 10, 0.9, pMut)
  val instGA2 = GeneticAlgorithm(inst2, rnd, 10, 0.9, pMut)
  val sol = instGA.solve(1000)
  println(sol)
}


object LoggerExampleGA extends App {
  // Use English formats
  import java.util.Locale
  Locale.setDefault(new  Locale.Builder().setLanguage("en").setRegion("US").build())

  java.util.Locale.setDefault(java.util.Locale.ENGLISH)
  val inst = Instance.fromFileOrLib("cap101.txt")
  val inst2 = Instance.random(0, 500, 150)
  val rnd = new Random(5)
  val pMut = 1.0/inst.numLocations
  val instGA = GeneticAlgorithm(inst, rnd, 10, 0.9, pMut)
  val instGA2 = GeneticAlgorithm(inst2, rnd, 10, 0.9, pMut)



  // Crea un logger, que permite registrar soluciones de tipo Double y que incluye un temporizador
  val logger = util.Logger[Double]()

  var iter = 0
  var best = Double.MaxValue

  // Ejecutar hasta 10 segundos
  while(logger.timer.elapsedTime() < 30.0) {
    val sol = instGA.solve(10)
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