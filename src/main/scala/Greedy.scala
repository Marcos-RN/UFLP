
class Greedy (val instance: Instance) {

  def solve : Solution = {
    val solution = Array.ofDim[Boolean](instance.numLocations)
    val evalSol = Array.ofDim[Double](instance.numLocations)
    val bestOption = Array.ofDim[Double](instance.numCustomers)
    for (i <- 0 until instance.numLocations) {
      val sol = Array.ofDim[Boolean](instance.numLocations)
      sol(i) = true
      evalSol(i) = Solution.apply(sol).eval(instance)
    }
    val first = evalSol.min
    println(first)
    val ind = evalSol.indexOf(first)
    solution(ind) = true
    println(solution.mkString(","))
    for (i <- bestOption.indices) {
      bestOption(i) = instance.serviceCost(i)(ind)
    }
    var c = 0
    while (c == 0){
      var total = Solution.apply(solution).eval(instance)
      var newFacVal = Solution.apply(solution).eval(instance)
      var newFacInd = 0
      for (i <- solution.indices) {
        var objValue = Solution.apply(solution).eval(instance)
        if (!solution(i)) {
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
      if (newFacVal < total) {
        total = newFacVal
        solution(newFacInd) = true
      }
      else c = 1
      println(solution.mkString(","))
    }
    Solution.apply(solution)
  }

  /*def solve : Solution = {
    val solution = Array.ofDim[Boolean](instance.numLocations)
    val evalSol = Array.ofDim[Double](instance.numLocations)
    for (i <- 0 until  instance.numLocations) {
      val sol = Array.ofDim[Boolean](instance.numLocations)
      sol(i) = true
      evalSol(i) = Solution.apply(sol).eval(instance)
    }
    val first = evalSol.min
    solution(evalSol.indexOf(first)) = true
    var c = 0
    while (c == 0) {
      val oldVal = Solution.apply(solution).eval(instance)
      val evalSol2 = Array.ofDim[Double](instance.numLocations)
      val arraySol = Array.ofDim[Array[Boolean]](instance.numLocations)
      for (k <- arraySol.indices) {
        arraySol(k) = solution
      }
      for (i <- arraySol.indices) {
        arraySol(i)(i) = true
        evalSol2(i) = Solution.apply(arraySol(i)).eval(instance)
      }
      val minVal = evalSol2.min
      //println(minVal)
      //En el siguiente paso del bucle, se cambian todos los elementos de la solucion a "true", cuando solo quiero cambiar el elemento que pertenece al lugar
      //correspondiente a la instalación que quiere abrir. En el GRASP pasa igual.
      if (minVal == oldVal) c = 1
      else solution(evalSol2.indexOf(minVal)) = true
      println(solution.mkString)
    }
    Solution.apply(solution)
  }*/

}

object Greedy {

  def apply(instance: Instance): Greedy = {
    new Greedy(instance)
  }
}

object greedyTest extends App {
  java.util.Locale.setDefault(java.util.Locale.ENGLISH)
  val inst = Instance.fromFile("instejemplo2.txt")
  val instGreedy = Greedy(inst)
  val sol = instGreedy.solve
  println(sol.eval(inst))
  //val inst2 = Instance.fromFileOrLib("cap71.txt")
  //val instGreedy2 = Greedy(inst2)
  //val sol2 = instGreedy2.solve
  //println(sol2.eval(inst2))
}
