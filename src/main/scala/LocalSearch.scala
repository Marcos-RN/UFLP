

class LocalSearch(val instance: Instance, val solution: Solution) {

  def neighbourSearch(bestOption : Array[Double]) : Solution = {
    var goOn = true
    var cont = 0
    var objValue = solution.objectiveValue
    val openFac = solution.openFacilities
    // 2 criterios de parada: que encuentre una solución mejor o que explore todas las posibilidades y no mejore
    while (goOn && cont < instance.numLocations) {
      for (i <- solution.openFacilities.indices) {
        //si está cerrada se "abre" y el proceso es igual que una iteración del algoritmo Greedy
        if (!openFac(i)) {
          var  newObjValue = objValue
          newObjValue += instance.openCost(i)
          for (j <- bestOption.indices) {
            if (instance.serviceCost(j)(i) < bestOption(j))
              newObjValue -= (bestOption(j)-instance.serviceCost(j)(i))
          }
          // si mejora se abre esta instalación, se actualiza el valor de la función objetivo y se detiene el bucle
          if (newObjValue < objValue) {
            objValue = newObjValue
            solution.openFacilities(i) = true
            goOn = false
          }
        }
        // si está abierta se "cierra" y se calcula de nuevo el valor de la función objetivo
        else {
          var  newObjValue = objValue
          newObjValue -= instance.openCost(i)
          for (j <- bestOption.indices) {
            // si el valor que hay en bestOption no se corresponde con el de la instalacion cerrada queda igual
            // por lo que solo se contemplan los casos en los que si que coincida
            if (instance.serviceCost(j)(i) == bestOption(j)) {
              newObjValue -= bestOption(j)
              var newVal = Double.MaxValue
              // se consideran los costes de servicio de todas las demas instalaciones, quedándonos con el mejor
              for (k <- 0 until i) {
                if (instance.serviceCost(j)(k) < newVal) {
                  newVal = instance.serviceCost(j)(k)
                }
              }
              for (k <- i+1 until instance.numLocations) {
                if (instance.serviceCost(j)(k) < newVal) {
                  newVal = instance.serviceCost(j)(k)
                }
              }
              newObjValue += newVal
            }
          }
          // si una vez hecho esto el nuevo valor de la función objetivo es mejor que el que había
          // se cierra esta instalación, se actualiza el valor de la función objetivo y se detiene el bucle
          if (newObjValue < objValue) {
            objValue = newObjValue
            solution.openFacilities(i) = false
            goOn = false
          }
        }
        cont += 1
      }
    }
    Solution(solution.openFacilities, objValue)
    }

  def HillClimbing : Solution = {
    //primero se crea el array de best option de la manera habitual
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
    var goOn = true
    var objValue = solution.objectiveValue
    var openFac = solution.openFacilities
    //el criterio de parada es que no encuentre una solución mejor, lo cual siempre acaba ocurriendo
    while (goOn) {
      var bO = bestOption
      //se aplica la funcion anterior a bestoption
      val sol = neighbourSearch(bO)
      //si devuelve el mismo valor objetivo, es decir, no mejora la solución, se termina el bucle
      if (sol.objectiveValue == objValue)
        goOn = false
      //si mejora la solución, se actualiza el valor de la función objetivo y el de bestoption
      //y continua el bucle
      else {
        objValue = sol.objectiveValue
        val bestOptionNew = Array.ofDim[Double](instance.numCustomers)
        for (i <- bestOptionNew.indices) {
          bestOptionNew(i) = Double.MaxValue
          for (j <- 0 until instance.numLocations) {
            if (sol.openFacilities(j)) {
              if (instance.serviceCost(i)(j) < bestOptionNew(i)) {
                bestOptionNew(i) = instance.serviceCost(i)(j)
              }
            }
          }
        }
        bO = bestOptionNew
        openFac = sol.openFacilities
      }
    }
    Solution(openFac,objValue)
  }
}

object LocalSearch {
  def apply(instance: Instance, solution: Solution): LocalSearch = {
    new LocalSearch(instance,solution)
  }
}


object LocalSearchTest extends App {
  java.util.Locale.setDefault(java.util.Locale.ENGLISH)
  val inst = Instance.fromFileOrLib("cap132.txt")
  val inst2 = Instance.random(0, 500, 150)
  val instGreedy = Greedy(inst)
  val instGreedy2 = Greedy(inst2)
  val sol = instGreedy.solve
  val sol2 = instGreedy2.solve
  println(sol)
  println()
  val local1 = LocalSearch(inst,sol)
  val local2 = LocalSearch(inst2,sol2)
  val hc1 = local1.HillClimbing
  val hc2 = local2.HillClimbing
  println(hc1)
}











