import java.io._
import util.random.xoShiRo256StarStar.Random

/*Implementation of Instance class for representing possible
  instances for UFLP.
  numLocations is the number of potential locations for opening facilities
  numCustomers is the number of customers to be served
  openCost is an Array with the open cost for a facility in each potential location
  serviceCost is an Array in which the position (i,j) represents the cost of serving customer i from location j
 */

class Instance (val numLocations: Int , val numCustomers: Int , val openCost: Array[Double] , val serviceCost : Array[Array[Double]]) {

  // It is possible to define this class using only openCost and serviceCost and using each length to fill
  // numLocations and numCustomers
  def this (openCost: Array[Double] , serviceCost : Array[Array[Double]]) {
    this (numLocations = openCost.length, numCustomers = serviceCost.length, openCost, serviceCost)
  }

  override def toString: String = s"Instance Number of potential locations = $numLocations , Instances Number of customers = $numCustomers"

  // Given an instance, generate a text file with the instance values
  def toFile(nombreFich: String): Unit = {
    val fich = new File(nombreFich)
    val ps = new PrintStream(fich)
    ps.println(numLocations)
    ps.println(numCustomers)
    ps.println()
    for (i <- 0 until numLocations) {
      ps.print(openCost(i))
      ps.print(" ")
    }
    ps.println()
    for (i <- 0 until numCustomers) {
      for (j <- 0 until numLocations){
        ps.print(serviceCost(i)(j))
        ps.print(" ")
      }
      ps.println()
    }
    ps.close()
  }

}

object Instance  {

  import java.util.Scanner

  def apply(numLocations: Int , numCustomers: Int , openCost: Array[Double] , serviceCost : Array[Array[Double]]) : Instance = {
    new Instance(numLocations, numCustomers, openCost, serviceCost)
  }
  def apply(openCost: Array[Double] , serviceCost : Array[Array[Double]]) : Instance = {
    new Instance(openCost, serviceCost)
  }


  // Read a text file with some parameters, in the OR-Library format
  // and return an Instance object with the given parameters
  def fromFileOrLib(nombreFich: String): Instance = {
    val fich = new File(nombreFich)
    val sc = new Scanner(fich)
    val line = sc.nextLine()
    val scLn = new Scanner(line)
    val numLocations = scLn.nextInt()
    val numCustomers = scLn.nextInt()
    scLn.close()
    val OC = Array.ofDim[Double](numLocations)
    var i = 0
    while (i < numLocations) {
      sc.nextDouble()
      OC(i) = sc.nextDouble()
      i += 1
    }
    val SC = Array.ofDim[Double](numCustomers, numLocations)
    var j = 0
    while (j < numCustomers) {
      sc.nextInt()
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

  // Read a text file with some parameters, in a customized format
  // and return an Instance object with the given parameters
  def fromFile(nombreFich: String): Instance = {
    val fich = new File(nombreFich)
    val sc = new Scanner(fich)
    val numLocations = sc.nextInt()
    val numCustomers = sc.nextInt()
    val OC = Array.ofDim[Double](numLocations)
    var i = 0
    while (i < numLocations) {
      OC(i) = sc.nextDouble()
      i += 1
    }
    val SC = Array.ofDim[Double](numCustomers, numLocations)
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

  //Gives a random Instance for the UFLP
  def random(rnd: Random, numLocations: Int, numCustomers: Int): Instance = {
    val openCost = Array.ofDim[Double](numLocations)
    val serviceCost = Array.ofDim[Double](numCustomers, numLocations)
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

  def random(seed: Int, numLocations: Int, numCustomers: Int): Instance =
    random(new Random(seed), numLocations, numCustomers)


}

object instanceTest extends App {
  java.util.Locale.setDefault(java.util.Locale.ENGLISH)
  val inst1 = Instance.fromFileOrLib("cap71.txt")
  //inst1.toFile("pruebaInstance")
  val inst2 = Instance.fromFile("instejemplo.txt")
  //inst2.toFile("ej")
}



