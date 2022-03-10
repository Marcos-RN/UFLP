import java.io._

class Instance (numFacilities: Int , numLocations: Int , openCost: Array[Double] , serviceCost : Array[Array[Double]]) {

  override def toString: String = s"Instance Number of facilities = $numFacilities , Instances Number of locations = $numLocations"

  def toFile(nombreFich: String): Unit = {
    val fich = new File(nombreFich)
    val ps = new PrintStream(fich)
    ps.println(numFacilities)
    ps.println(numLocations)
    ps.println()
    ps.println("{")
    for (i <- 0 until numFacilities) {
      ps.print(openCost(i))
    }
    ps.println("}")
    ps.println()
    ps.println("{")
    for (i <- 0 until numLocations) {
      ps.println("{")
      for (j <- 0 until numFacilities){
        ps.print(serviceCost(i)(j))
      }
      ps.println("}")
    }
    ps.close()
  }

}

  object Instance extends App {

    import java.util.Scanner

    def fromFileOrLib(nombreFich: String): Unit = {
      var NF = 0
      var NL = 0
      val fich = new File(nombreFich)
      val sc = new Scanner(fich)
      NF = sc.nextInt()
      NL = sc.nextInt()
      var OC = Array.ofDim[Double](NF)
      var i = 0
      while (i < NF) {
        val line = sc.nextLine()
        val scLn = new Scanner(line)
        OC(i) = scLn.nextDouble()
        i += 1
        scLn.close()
      }
      OC
      var SC = Array.ofDim[Double](NL, NF)
      var j = 0
      while (j < NL) {
        sc.nextLine()
        val line = sc.nextLine()
        val scLn = new Scanner(line)
        var k = 0
        while (k < NF) {
          SC(j)(k) = scLn.nextDouble()
          k += 1
        }
        j += 1
      }
      sc.close()
      //return Instance(NF, NL, SC, OC)
    }

  }




