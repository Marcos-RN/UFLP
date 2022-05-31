

class LocalSearch(val instance: Instance, val solution: Solution) {

  def neighbourSearch(bestOption : Array[Double], openFac : Array[Boolean], objValue: Double) : Solution = {
    var goOn = true
    var i = 0
    var value = objValue
    // 2 criterios de parada: que encuentre una solución mejor o que explore todas las posibilidades y no mejore
    while (goOn && i < instance.numLocations) {
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
            value = newObjValue
            openFac(i) = true
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
                if (openFac(k) && instance.serviceCost(j)(k) < newVal) {
                  newVal = instance.serviceCost(j)(k)
                }
              }
              for (k <- i+1 until instance.numLocations) {
                if (openFac(k) && instance.serviceCost(j)(k) < newVal) {
                  newVal = instance.serviceCost(j)(k)
                }
              }
              newObjValue += newVal
            }
          }
          // si una vez hecho esto el nuevo valor de la función objetivo es mejor que el que había
          // se cierra esta instalación, se actualiza el valor de la función objetivo y se detiene el bucle
          if (newObjValue < objValue) {
            value = newObjValue
            openFac(i) = false
            goOn = false
          }
        }
        i += 1
      }
    Solution(openFac, value)
    }


  def HillClimbing : Solution = {
    //primero se crea el array de best option de la manera habitual
    var objValue = solution.objectiveValue
    var openFac = solution.openFacilities
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
    //el criterio de parada es que no encuentre una solución mejor, lo cual siempre acaba ocurriendo
    while (goOn) {
      //se aplica la funcion anterior a bestoption
      val sol = neighbourSearch(bestOption, openFac, objValue)
      //si devuelve el mismo valor objetivo, es decir, no mejora la solución, se termina el bucle
      if (sol.objectiveValue == objValue)
        goOn = false
      //si mejora la solución, se actualiza el valor de la función objetivo, de las instalaciones abiertas
      // y el de bestoption, y continua el bucle
      else {
        objValue = sol.objectiveValue
        openFac = sol.openFacilities
        for (i <- bestOption.indices) {
          bestOption(i) = Double.MaxValue
          for (j <- 0 until instance.numLocations) {
            if (openFac(j)) {
              if (instance.serviceCost(i)(j) < bestOption(i)) {
                bestOption(i) = instance.serviceCost(i)(j)
              }
            }
          }
        }
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
  val inst = Instance.fromFileOrLib("cap72.txt")
  val inst2 = Instance.random(7, 150, 150)
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











