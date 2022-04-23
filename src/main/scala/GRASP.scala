import util.random.xoShiRo256StarStar.Random

import scala.collection.mutable.ArrayBuffer

class GRASP (val instance: Instance) {

  "NO FUNCIONA BIEN"

  def solve(n: Int) : Solution = {
    val solution = Array.ofDim[Boolean](instance.numLocations)
    val evalSol = Array.ofDim[Double](instance.numLocations)
    val bestOption = Array.ofDim[Double](instance.numCustomers)
    for (i <- 0 until instance.numLocations) {
      var sum = 0.0
      for (j <- 0 until instance.numCustomers) {
        sum += instance.serviceCost(j)(i)
      }
      evalSol(i) = sum
    }
    val first = evalSol.min
    println(first)
    val ind = evalSol.indexOf(first)
    solution(ind) = true
    println(solution.mkString(","))
    for (i <- bestOption.indices) {
      bestOption(i) = instance.serviceCost(i)(ind)
    }
    var seed = 0
    var goOn = true
    var solValue = first
    while (goOn){
      var total = solValue
      var newFacVal = solValue
      val potentialFac = ArrayBuffer[Double]()
      for (i <- solution.indices) {
        var objValue = solValue
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
      println(potentialFac.mkString(","))
      val nSel = potentialFac.sorted.take(n)
      println(nSel.mkString(","))
      val rnd = Random(seed)
      val chosen = rnd.uniform(n)
      println(chosen)
      newFacVal = nSel(chosen)
      println(newFacVal)
      val ind = potentialFac.indexOf(newFacVal)
      if (newFacVal < total) {
        total = newFacVal
        solution(ind) = true
        for (i <- bestOption.indices) {
          if (instance.serviceCost(i)(ind) < bestOption(i))
            bestOption(i) = instance.serviceCost(i)(ind)
        }
        solValue = newFacVal
      }
      else goOn = false
      seed += 1
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
  //val inst = Instance.fromFile("instejemplo.txt")
  //val instGrasp = GRASP(inst)
 // val sol = instGrasp.solve(2, 1)
  //println(sol.eval(inst))
  val inst2 = Instance.fromFileOrLib("cap102.txt")
  val instGrasp2= GRASP(inst2)
  val sol2 = instGrasp2.solve(5)
  println(sol2.eval(inst2))
  println(sol2)
}