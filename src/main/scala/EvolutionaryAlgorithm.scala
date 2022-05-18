import util.Logger
import util.random.xoShiRo256StarStar.Random

abstract class EvolutionaryAlgorithm(instance: Instance, rnd: Random, popSz: Int, pX: Double, pMut: Double) {
  require(0 < pX && pX < 1, "Cross probability must be between 0 and 1")
  require(0 < pMut && pMut < 1, "Mutation probability must be between 0 and 1")

  val logger: Logger[Double] = Logger[Double]()

  class Individual {
    val solution: Array[Boolean] = Array.fill[Boolean](instance.numLocations)(false)
    var fitness: Double = Double.MaxValue // aquí guardaríamos el valor objetivo de la solución

    def computeFitness: Double = Solution.eval(solution, instance) // Este método calcularía el valor objetivo de la solución almacenada en solution
  }

  private val population = Array.fill[Individual](popSz)(new Individual)

  // Aquí tendremos un hijo en cada iteración
  private var child = new Individual

  def initialization(): Unit

  def randomInitialization(): Unit = {
    // Aquí rellenaríamos cada uno de los individuos del array population
    // calcularíamos su fitness, lo asignaríamos al campo fitness y  ordenaríamos
    // en orden descendente por fitness.
    for (i <- population.indices) {
      for (j <- population(i).solution.indices) {
        val r = rnd.uniform(0.0, 1.0)
        if (r < 0.5)
          population(i).solution(j) = true
      }

      population(i).fitness = population(i).computeFitness
    }
    // Esta línea ordena population ascendentemente por fitness
    scala.util.Sorting.quickSort(population)(Ordering.by(sol => sol.fitness))
  }

  def graspInitialization(): Unit = {
    // Aquí rellenaríamos cada uno de los individuos del array population
    // calcularíamos su fitness, lo asignaríamos al campo fitness y  ordenaríamos
    // en orden descendente por fitness.
    for (i <- population.indices) {
      val instGrasp = GRASP(instance, rnd)
      val graspSol = instGrasp.solve(8)
      for (j <- graspSol.openFacilities.indices)
        population(i).solution(j) = graspSol.openFacilities(j)
      population(i).fitness = graspSol.objectiveValue
    }
    // Esta línea ordena population ascendentemente por fitness
    scala.util.Sorting.quickSort(population)(Ordering.by(sol => sol.fitness))
  }

  def selection(): Int

  // devuelve el índice de un padre seleccionado por torneo binario
  def binaryTournament(): Int = {
    val a = rnd.uniform(popSz)
    val b = rnd.uniform(popSz)
    val index = a min (b)
    index
  }

  def crossover(indexParent1: Int, indexParent2: Int): Unit

  // dados los índices de los dos padres, mete en child el individuo cruzado
  def onePointCrossover(indexParent1: Int, indexParent2: Int): Unit = {
    val Parent1 = population(indexParent1)
    val Parent2 = population(indexParent2)
    val cut = rnd.uniform(instance.numLocations)
    for (i <- 0 until cut)
      child.solution(i) = Parent1.solution(i)
    for (i <- cut until instance.numLocations)
      child.solution(i) = Parent2.solution(i)
  }

  def uniformCrossover(indexParent1: Int, indexParent2: Int): Unit = {
    val Parent1 = population(indexParent1)
    val Parent2 = population(indexParent2)
    for (i <- child.solution.indices) {
      val r = rnd.uniform(0.0, 1.0)
      if (r < 0.5)
        child.solution(i) = Parent1.solution(i)
      else
        child.solution(i) = Parent2.solution(i)
    }
  }

  // copia un individuo arbitrario de la población a child, por si no se realizara cruce
  def copyToChild(): Unit = {
    val source = population(rnd.uniform(popSz))
    for(i <- child.solution.indices)
      child.solution(i) = source.solution(i)
  }

  def mutate(): Unit

  // muta el individuo almacenado en child
  def flipMutation(): Unit = {
    for (i <- child.solution.indices) {
      val r = rnd.uniform(0.0, 1.0)
      if (r < pMut)
        child.solution(i) = !child.solution(i)
    }
  }

  def replacement(): Unit

  // reemplaza el peor individuo de la población con el hijo
  // y reordena la población usando búsqueda binaria.
  def replaceWorst(): Unit = {
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

    val replaced = population(popSz - 1)
    for(i <- popSz - 1 to pos + 1 by -1)
      population(i) = population(i - 1)

    population(pos) = child
    child = replaced
  }

  def postProcess(): Unit

  def solve(maxIters : Int) : Solution = {
    initialization()
    var iter = 0
    while (iter < maxIters) {
      val r1 = rnd.uniform(0.0, 1.0)
      if (r1 < pX) {
        val p1 = selection()
        val p2 = selection()
        crossover(p1, p2)
      }
      else
        copyToChild()
      mutate()
      child.fitness = child.computeFitness
      postProcess()
      replacement()
      iter += 1
    }
    val bestSol = Solution.apply(population(0).solution, population(0).fitness)
    bestSol
  }


  def solve(maxTime : Double) : Solution = {
    logger.timer.reset()
    initialization()
    var bestFitness = population(0).fitness
    var iter = 0
    logger.register("iter: %20d   fitness: %20.8f   time: %20.8f", iter, bestFitness)
    while (logger.timer.elapsedTime() < maxTime) {
      val r = rnd.uniform(0.0, 1.0)
      if (r < pX) {
        val p1 = selection()
        val p2 = selection()
        crossover(p1, p2)
      }
      else
        copyToChild()
      mutate()
      child.fitness = child.computeFitness
      postProcess()
      replacement()
      iter += 1

      if(population(0).fitness < bestFitness) {
        bestFitness = population(0).fitness
        logger.register("iter: %20d   fitness: %20.8f   time: %20.8f", iter, bestFitness)
      }
    }
    val bestSol = Solution.apply(population(0).solution, population(0).fitness)
    bestSol
  }
}


class RandomBinaryUniformFlipWorstEA(instance: Instance, rnd: Random, popSz: Int, pX: Double, pMut: Double)
  extends EvolutionaryAlgorithm(instance, rnd, popSz, pX, pMut) {

  override def initialization(): Unit =
    randomInitialization()

  override def selection(): Int =
    binaryTournament()

  override def crossover(indexParent1: Int, indexParent2: Int): Unit =
    uniformCrossover(indexParent1, indexParent2)

  override def mutate(): Unit =
    flipMutation()

  override def replacement(): Unit =
    replaceWorst()

  override def postProcess(): Unit = {}
}


object RandomBinaryUniformFlipWorstEA {
  def apply(instance: Instance, rnd: Random, popSz: Int, pX: Double, pMut: Double) : EvolutionaryAlgorithm = {
    new RandomBinaryUniformFlipWorstEA(instance, rnd, popSz, pX, pMut)
  }
}


class RandomBinaryOnePointFlipWorstEA(instance: Instance, rnd: Random, popSz: Int, pX: Double, pMut: Double)
  extends EvolutionaryAlgorithm(instance, rnd, popSz, pX, pMut) {

  override def initialization(): Unit =
    randomInitialization()

  override def selection(): Int =
    binaryTournament()

  override def crossover(indexParent1: Int, indexParent2: Int): Unit =
    onePointCrossover(indexParent1, indexParent2)

  override def mutate(): Unit =
    flipMutation()

  override def replacement(): Unit =
    replaceWorst()

  override def postProcess(): Unit = {}
}


object RandomBinaryOnePointFlipWorstEA {
  def apply(instance: Instance, rnd: Random, popSz: Int, pX: Double, pMut: Double) : EvolutionaryAlgorithm = {
    new RandomBinaryUniformFlipWorstEA(instance, rnd, popSz, pX, pMut)
  }
}


object EATest extends App {
  // Use English formats
  import java.util.Locale
  Locale.setDefault(Locale.ENGLISH)

  def testEA(instance: Instance, seed: Int): Unit = {
    val pX = 0.9
    val pMut = 1.0 / instance.numLocations
    val popSz = 100
    val maxTime = 80.0 // seconds
    val rnd = Random(seed)
    val ea = RandomBinaryUniformFlipWorstEA(instance, rnd, popSz, pX, pMut)
    val solution = ea.solve(maxTime)
    println(solution)
    ea.logger.print()
  }

  testEA(ExampleInstances.inst8, 0)
}
