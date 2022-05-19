import Instance.fromFileOrLib

import java.io._
import util.random.xoShiRo256StarStar.Random

class Instance (val numLocations: Int , val numCustomers: Int , val openCost: Array[Double] , val serviceCost : Array[Array[Double]]) {

  def this (openCost: Array[Double] , serviceCost : Array[Array[Double]]) {
    this (numLocations = openCost.length, numCustomers = serviceCost.length, openCost, serviceCost)
  }

  override def toString: String = s"Instance Number of potential locations = $numLocations , Instances Number of customers = $numCustomers"

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

  def fromFileSimple(nombreFich: String): Instance = {
    val fich = new File(nombreFich)
    val sc = new Scanner(fich)
    sc.nextLine()
    val line = sc.nextLine()
    val scLn = new Scanner(line)
    val numLocations = scLn.nextInt()
    val numCustomers = scLn.nextInt()
    scLn.nextInt()
    scLn.close()
    val OC = Array.ofDim[Double](numLocations)
    val SC = Array.ofDim[Double](numCustomers, numLocations)

    while (sc.hasNextLine) {
      val line = sc.nextLine()
      val scLn = new Scanner(line)
      val location = scLn.nextInt() - 1
      OC(location) = scLn.nextDouble()
      for(j <- 0 until numCustomers)
        SC(j)(location) = scLn.nextDouble()
      scLn.close()
    }
    sc.close()
    Instance(numLocations, numCustomers, OC, SC)
  }

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



  def randomUniform( rnd: Random, numLocations: Int, numCustomers: Int
                   , minOpenCost: Double, maxOpenCost: Double
                   , minServiceCost: Double, maxServiceCost: Double
                   ): Instance = {
    val openCost = Array.ofDim[Double](numLocations)
    val serviceCost = Array.ofDim[Double](numCustomers, numLocations)
    for (i <- 0 until numLocations) {
      openCost(i) = rnd.uniform(minOpenCost, maxOpenCost)
    }
    for (j <- 0 until numCustomers) {
      for (k <- 0 until numLocations) {
        serviceCost(j)(k) = rnd.uniform(minServiceCost, maxServiceCost)
      }
    }
    Instance(numLocations, numCustomers, openCost, serviceCost)
  }

  def randomUniform( seed: Int, numLocations: Int, numCustomers: Int
                   , minOpenCost: Double, maxOpenCost: Double
                   , minServiceCost: Double, maxServiceCost: Double
                   ): Instance =
    randomUniform( Random(seed), numLocations, numCustomers
                 , minOpenCost, maxOpenCost
                 , minServiceCost, maxServiceCost
                 )
}

object instanceTest extends App {
  java.util.Locale.setDefault(java.util.Locale.ENGLISH)
  val inst1 = Instance.fromFileOrLib("cap71.txt")
  //inst1.toFile("pruebaInstance")
  val inst2 = Instance.fromFile("instejemplo.txt")
  //inst2.toFile("ej")
}



object ExampleInstances {

  lazy val inst1 = Instance.fromFileOrLib("data/ORlib/cap74.txt")
  lazy val inst2 = Instance.random(0, 500, 250)

  lazy val inst3 = Instance.randomUniform(0, 250, 1000, 100000, 500000, 1000, 5000)
  lazy val inst4 = Instance.fromFileOrLib("data/ORlib/M/T/MT1") //*
  lazy val inst5 = Instance.fromFileOrLib("data/ORlib/M/S/MS1")
  lazy val inst6 = Instance.fromFileOrLib("data/ORlib/M/R/MR5")

  lazy val inst7 = Instance.fromFileSimple("data/Simple/kmedian/1000-10")
  lazy val inst8 = Instance.fromFileSimple("data/Simple/KoerkelGhoshAsymmetric/750/a/ga750a-1")
}