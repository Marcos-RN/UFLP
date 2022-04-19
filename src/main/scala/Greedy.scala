
class Greedy (val instance: Instance) {

  def solve : Solution = {
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
      println(minVal)
      if (minVal == oldVal) c = 1
      else solution(evalSol2.indexOf(minVal)) = true
      println(solution.mkString)
    }
    Solution.apply(solution)
  }

}

object Greedy extends App {

  def apply(instance: Instance): Greedy = {
    new Greedy(instance)
  }
}

object greedyTest extends App {
  val inst = Instance.fromFile("instejemplo.txt")
  val instGreedy = Greedy(inst)
  val sol = instGreedy.solve
  println(sol.eval(inst))
  //val inst2 = Instance.fromFileOrLib("cap71.txt")
  //val instGreedy2 = Greedy(inst2)
  //val sol2 = instGreedy2.solve
  //println(sol2.eval(inst2))
}
