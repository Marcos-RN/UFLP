import java.io.File

class Instance (NF: Int , NL: Int , OC: Array[Int] , SC : Array[Array[Int]]) {

  val openCost: Array[Int] = OC
  val serviceCost: Array[Array[Int]] = SC
  val NumFacilities: Int = NF
  val NumLocations: Int = NL


  override def toString: String = s"Instance Number of facilities = $NumFacilities , Instances Number of locations = $NumLocations"

  object Leer extends App {
    import java.util.Scanner

    def parametros(nombreFich : String) : (Int,Int,Array[Double],Array[Array[Double]]) = {
      var NF = 0
      var NL = 0
      var OC = Array[Double]()
      var SC = Array[Array[Double]]()
      val fich = new File(nombreFich)
      val sc = new Scanner(fich)
      NF = sc.nextInt()
      NL = sc.nextInt()
      var i = 1
      while (i <= NF) {
        val line = sc.nextLine()
        val scLn = new Scanner(line)
        scLn.next()
        OC = OC ++ Array[scLn.nextDouble()]
        i += 1
        scLn.close()
      }
      var j = 1
      while (j <= NL) {
        sc.nextLine()
        var L = Array[Double]()
        val line = sc.nextLine()
        val scLn = new Scanner(line)
        var k = 1
        while (k <= NF) {
          L = L ++ Array[scLn.nextDouble()]
          k +=1
        }
        SC = SC ++ L
        j += 1
      }
      sc.close()
      (NF,NL,OC,SC)
    }
  }


}
