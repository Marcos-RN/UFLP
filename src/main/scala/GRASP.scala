import util.random.xoShiRo256StarStar.Random

class GRASP (val instance: Instance) {

  def solve(n: Int) : Solution = {
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
      val sortedEval = evalSol2.sorted
      val minValues = sortedEval.take(n)
      val seed = ((instance.openCost.sum)/(instance.numLocations)).toInt
      val rnd = Random(seed)
      val chosen = rnd.uniform(n)
      val minVal = minValues(chosen)
      //println(minVal)
      if (minVal == oldVal) c += 1
      else solution(evalSol2.indexOf(minVal)) = true
      println(solution.mkString)
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
  val sol = instGrasp.solve(3)
  println(sol.eval(inst))
  val inst2 = Instance.fromFileOrLib("cap71.txt")
  val instGrasp2= GRASP(inst2)
  val sol2 = instGrasp2.solve(5)
  println(sol2.eval(inst2))
}