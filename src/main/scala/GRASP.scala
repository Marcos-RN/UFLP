import util.random.xoShiRo256StarStar.Random

import scala.collection.mutable.ArrayBuffer

class GRASP (val instance: Instance) {

  "NO FUNCIONA BIEN"

  def solve(n: Int, seed: Int) : Solution = {
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
      val potentialFac = ArrayBuffer[Double]()
      for (i <- solution.indices) {
        var objValue = Solution.apply(solution).eval(instance)
        if (!solution(i)) {
          objValue += instance.openCost(i)
          for (j <- bestOption.indices) {
            if (instance.serviceCost(j)(i) < bestOption(j))
              objValue -= (bestOption(j)-instance.serviceCost(j)(i))
          }
          if (objValue < total)
            potentialFac += objValue
            else potentialFac += Double.MaxValue
        }
      }
      val nSel = potentialFac.sorted.take(n)
      val rnd = Random(seed)
      val chosen = rnd.uniform(n)
      newFacVal = nSel(chosen)
      val ind = potentialFac.indexOf(newFacVal)
      if (newFacVal < total) {
        total = newFacVal
        solution(ind) = true
      }
      else c = 1
      println(solution.mkString(","))
    }
    Solution.apply(solution)
  }



}

object GRASP {

  def apply(instance: Instance): GRASP = {
    new GRASP(instance)
  }

}

object graspTest extends App {
  java.util.Locale.setDefault(java.util.Locale.ENGLISH)
  val inst = Instance.fromFile("instejemplo.txt")
  val instGrasp = GRASP(inst)
  val sol = instGrasp.solve(2, 1)
  println(sol.eval(inst))
  //val inst2 = Instance.fromFileOrLib("cap73.txt")
  //val instGrasp2= GRASP(inst2)
  //val sol2 = instGrasp2.solve(5)
  //println(sol2.eval(inst2))
}