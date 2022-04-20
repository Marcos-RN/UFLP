import util.random.xoShiRo256StarStar.Random

import java.io.File

object Hola extends App{
  println("hola")
}
object arr extends App{
  var A = Array[Int]()
  var B = Array[Int]()
  A = Array(5,25,63,1,9,100)
  B = Array(4,8)
  var C = A.sorted
  println(A.mkString)
  println(C.mkString)
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

object randomTest extends App {

  val rnd = Random(1)
  for (i <- 0 to 10) {
    println(rnd.uniform(10))
  }
}
