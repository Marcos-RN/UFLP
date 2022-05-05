import util.random.xoShiRo256StarStar.Random

import scala.collection.mutable.PriorityQueue

class GRASP (val instance: Instance , val rnd : Random) {

  case class PQItem(index: Int, value : Double) extends Ordered[PQItem] {
    def compare(that: PQItem): Int = -this.value.compare(that.value)
  }

  def min(a : Int, b: Int): Int = {
    if (a < b) a
    else b
  }

  def solve(n: Int) : Solution = {
    val solution = Array.ofDim[Boolean](instance.numLocations)
    val evalSol = Array.ofDim[Double](instance.numLocations)
    val bestOption = Array.ofDim[Double](instance.numCustomers)
    val pq = new  PriorityQueue[PQItem]()
    for (i <- 0 until instance.numLocations) {
      var sum = 0.0
      for (j <- 0 until instance.numCustomers) {
        sum += instance.serviceCost(j)(i)
      }
      sum += instance.openCost(i)
      evalSol(i) = sum
      pq.enqueue(PQItem(i,sum))
    }
    val m = min(n,instance.numLocations)
    val chosen = rnd.uniform(m)
    var first : PQItem = null
    for (i <- 0 to chosen) {
      first = pq.dequeue()
    }
    pq.clear()
    println(first.value)
    solution(first.index) = true
    println(solution.mkString(","))
    for (i <- bestOption.indices) {
      bestOption(i) = instance.serviceCost(i)(first.index)
    }
    var goOn = true
    var solValue = first.value
    while (goOn) {
      var newFacVal = solValue
      for (i <- solution.indices) {
        if (!solution(i)) {
          var objValue = solValue
          for (j <- bestOption.indices) {
            objValue += instance.openCost(i)
            if (instance.serviceCost(j)(i) < bestOption(j))
              objValue -= (bestOption(j) - instance.serviceCost(j)(i))
          }
          if (objValue < solValue)
            pq.enqueue(PQItem(i, objValue))
        }
      }
      if (pq.isEmpty) goOn = false
      else {
        val m = min(n, pq.size)
        val chosen = rnd.uniform(m)
        var pqSel : PQItem = null
        for (k <- 0 to chosen) {
          pqSel = pq.dequeue()
        }
        newFacVal = pqSel.value
        val ind = pqSel.index
        pq.clear()
        if (newFacVal < solValue) {
          solution(ind) = true
          for (i <- bestOption.indices) {
            if (instance.serviceCost(i)(ind) < bestOption(i))
              bestOption(i) = instance.serviceCost(i)(ind)
          }
          solValue = newFacVal
        }
        else goOn = false
        println(solution.mkString(","))
      }
    }
    Solution.apply(solution)
  }

}

object GRASP {

  def apply(instance: Instance, rnd: Random): GRASP = {
    new GRASP(instance, rnd)
  }

}

object graspTest extends App {
  java.util.Locale.setDefault(java.util.Locale.ENGLISH)
  val inst = Instance.fromFileOrLib("cap74.txt")
  val rnd = new Random(6)
  val instGrasp2= GRASP(inst, rnd)
  val sol2 = instGrasp2.solve(5)
  println(sol2.eval(inst))
  println(sol2)
}