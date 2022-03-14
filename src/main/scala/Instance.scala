import java.io._
import util.random.xoShiRo256StarStar.Random

class Instance (numLocations: Int , numCustomers: Int , openCost: Array[Double] , serviceCost : Array[Array[Double]]) {

  override def toString: String = s"Instance Number of potential locations = $numLocations , Instances Number of customers = $numCustomers"

  def toFile(nombreFich: String): Unit = {
    val fich = new File(nombreFich)
    val ps = new PrintStream(fich)
    ps.println(numLocations)
    ps.println(numCustomers)
    ps.println()
    ps.println("{")
    for (i <- 0 until numLocations) {
      ps.print(openCost(i))
    }
    ps.print("}")
    ps.println()
    ps.println("{")
    for (i <- 0 until numCustomers) {
      ps.println("{")
      for (j <- 0 until numLocations){
        ps.print(serviceCost(i)(j))
      }
      ps.print("}")
    }
    ps.println("}")
    ps.close()
  }

}

  object Instance extends App {

    import java.util.Scanner

    def apply(numLocations: Int , numCustomers: Int , openCost: Array[Double] , serviceCost : Array[Array[Double]]) : Instance = {
      new Instance(numLocations, numCustomers, openCost, serviceCost)
    }

    def fromFileOrLib(nombreFich: String): Unit = {
      var numLocations = 0
      var numCustomers = 0
      val fich = new File(nombreFich)
      val sc = new Scanner(fich)
      numLocations = sc.nextInt()
      numCustomers = sc.nextInt()
      var OC = Array.ofDim[Double](numLocations)
      var i = 0
      while (i < numLocations) {
        val line = sc.nextLine()
        val scLn = new Scanner(line)
        OC(i) = scLn.nextDouble()
        i += 1
        scLn.close()
      }
      OC
      var SC = Array.ofDim[Double](numCustomers, numLocations)
      var j = 0
      while (j < numCustomers) {
        sc.nextLine()
        val line = sc.nextLine()
        val scLn = new Scanner(line)
        var k = 0
        while (k < numLocations) {
          SC(j)(k) = scLn.nextDouble()
          k += 1
        }
        j += 1
      }
      sc.close()
      Instance(numLocations, numCustomers, OC, SC)
    }

    def fromFile(nombreFich: String): Unit = {
      var numLocations = 0
      var numCustomers = 0
      val fich = new File(nombreFich)
      val sc = new Scanner(fich)
      numLocations = sc.nextInt()
      numCustomers = sc.nextInt()
      var OC = Array.ofDim[Double](numLocations)
      var i = 0
      while (i < numLocations) {
        OC(i) = sc.nextDouble()
        i += 1
      }
      OC
      var SC = Array.ofDim[Double](numCustomers, numLocations)
      var j = 0
      while (j < numCustomers) {
        var k = 0
        while (k < numLocations) {
          SC(j)(k) = sc.nextDouble()
          k += 1
        }
        j += 1
      }
      sc.close()
      Instance(numLocations, numCustomers, OC, SC)
    }

    def random1(rnd: Random, numLocations: Int, numCustomers: Int): Instance = {
      var openCost = Array.ofDim[Double](numLocations)
      var serviceCost = Array.ofDim[Double](numCustomers, numLocations)
      val OC = rnd.nextInt().abs
      for (i <- 0 until numLocations) {
        openCost(i) = OC
      }
      for (j <- 0 until numCustomers) {
        for (k <- 0 until numLocations) {
          serviceCost(j)(k) = rnd.gaussian(OC, OC / 4)
        }
      }
      Instance(numLocations, numCustomers, openCost, serviceCost)
    }


    def random(seed: Int, numFacilities: Int, numLocations: Int): Instance
    =
      random1(new Random(seed), numFacilities, numLocations)
  }




