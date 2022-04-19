import java.io.File

object Hola extends App{
  println("hola")
}
object arr extends App{
  var A = Array[Int]()
  var B = Array[Int]()
  A = Array(2,5,6)
  B = Array(4,8)
  println(A)
}

object file extends App {
def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
  val p = new java.io.PrintWriter(f)
  try {op(p)} finally {p.close()}
}
  import java.io._
  val data = Array("Five","strings","in","a","file!")
  printToFile(new File("example.txt")) { p =>
    data.foreach(p.println)
  }

}
