import util.random.xoShiRo256StarStar.Random

class GeneticAlgorithm(instance: Instance, rnd: Random, popSz: Int, pX: Double, pMut: Double) {

  require(0 < pX && pX < 1, "Cross probability must be between 0 and 1")
  require(0 < pMut && pMut < 1, "Mutation probability must be between 0 and 1")


  class Individual {
    var solution = Array.fill[Boolean](instance.numLocations)(false)
    var fitness: Double = Double.MaxValue // aquí guardaríamos el valor objetivo de la solución

    def computeFitness: Double = Solution.eval(solution, instance) // Este método calcularía el valor objetivo de la solución almacenada en solution
  }

  private val population = Array.fill[Individual](popSz)(new Individual)

  // Aquí tendremos un hijo en cada iteración
  private val child = new Individual

  def initialization(): Unit = {
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
    // Esta línea ordena population descendentemente (al estar negado) por fitness
    scala.util.Sorting.quickSort(population)(Ordering.by(sol => -sol.fitness))
  }

  def initializationGRASP(): Unit = {
    // Aquí rellenaríamos cada uno de los individuos del array population
    // calcularíamos su fitness, lo asignaríamos al campo fitness y  ordenaríamos
    // en orden descendente por fitness.
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
    // Esta línea ordena population descendentemente (al estar negado) por fitness
    scala.util.Sorting.quickSort(population)(Ordering.by(sol => -sol.fitness))
  }

  // devuelve el índice de un padre seleccionado por torneo binario
  def binaryTournament(): Int = {
    val a = rnd.uniform(popSz)
    val b = rnd.uniform(popSz)
    val index = a min (b)
    index
  }

  // dados los índices de los dos padres, mete en child el individuo cruzado
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

  // copia un individuo arbitrario de la población a child, por si no se realizara cruce
  def copyToChild(): Unit = {
    val a = rnd.uniform(popSz)
    child.solution = population(a).solution
    child.fitness = population(a).fitness
  }

  // muta el individuo almacenado en child
  def mutate(): Unit = {
    for (i <- child.solution.indices) {
      val r = rnd.uniform(0.0, 1.0)
      if (r < pMut)
        child.solution(i) = !child.solution(i)
    }
  }

  // reemplaza el peor individuo de la población con el hijo
  // y reordena la población usando búsqueda binaria.
  def replacement(): Unit = {
    var pos = 0
    if (child.fitness < population(0).fitness)
      pos = 0
    else {
      if (child.fitness > population(popSz - 1).fitness)
        pos = popSz - 1
      else {
        var min = 0
        var max = instance.numLocations - 2
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

  def solve(maxIter : Int) : Unit = {
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
  }

}
