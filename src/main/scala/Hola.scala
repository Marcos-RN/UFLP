import java.io.File

object Hola extends App{
  println("hola")
}
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
      val línea = sc.nextLine()
      val scLn = new Scanner(línea)
      scLn.next()
      OC = OC ++ Array[scLn.nextDouble()]
      i += 1
      scLn.close()
    }
    var j = 1
    while (j <= NL) {
      sc.nextLine()
      var L = Array[Double]()
      val línea = sc.nextLine()
      val scLn = new Scanner(línea)
      var k = 1
      while (k <= NF) {
        L = L ++ Array[scLn.nextDoble()]
        k +=1
      }
      SC = SC ++ L
      j += 1
    }
    sc.close()
    return (NF,NL,OC,SC)

  }

  println(parametros("cap71.txt"))
}
