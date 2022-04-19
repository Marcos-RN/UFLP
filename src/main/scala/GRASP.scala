import util.random.xoShiRo256StarStar.Random
import scala.Array

class GRASP (val instance: Instance) {

  //def solve(n: Int) : Solution = {}


}

object GRASP extends App {



  def compare(total : Double, chosenCost : Array[Double], nextCost : Array[Double], openCost : Array[Double], step : Int) : (Array[Double], Double) = {
    val newCost = Array.ofDim[Double](chosenCost.length)
    for (i <- chosenCost.indices) {
      if (nextCost(i) < chosenCost(i)) (newCost(i) = nextCost(i))
      else newCost(i) = chosenCost(i)
    }
    val newTotal = newCost.sum + openCost(step)
    (if (newTotal < total) (newCost,newTotal)
    else (chosenCost, total))
  }

  def firstGrasp(instance: Instance) : Array[Boolean] = {
    val openCost = instance.openCost
    val serviceCost = instance.serviceCost
    val sol = Array.ofDim[Boolean](instance.numLocations)
    sol(0) = true
    var total = serviceCost(0).sum + openCost(0)
    var chosenCost = serviceCost(0)
    for (i <- 1 until instance.numLocations) {
      val newStep = compare(total, chosenCost, serviceCost(i), openCost, i)
      chosenCost = newStep._1
      if (newStep._2 == total) sol(i) = false
      else sol(i) = true
      total = newStep._2
      println(total)
    }
    sol
  }
}
object graspTest extends App {
  java.util.Locale.setDefault(java.util.Locale.ENGLISH)
  val inst1 = Instance.fromFileOrLib("cap71.txt")
  inst1.toFile("pruebaInstance")
  GRASP.firstGrasp(inst1)

  val grasp1 = new GRASP(inst1)
  //val sol1 = grasp1.solve()
}