/******************************************************************************
 *
 * Class to log evolution of solutions along execution time
 *
 * @ Pepe Gallardo, 2016
 *
 *****************************************************************************/

package util

import scala.collection.mutable.ArrayBuffer

case class Logger[Fitness]( val timer : Timer = new Timer()
                          , echo : Boolean = true
                          ) {

  import Timer.Seconds

  def fitness2Format(fitness: Fitness): String = fitness match {
    case i : Int    => "%d"
    case f : Float  => "%.8f"
    case d : Double => "%.8f"
    case _          => "%s"
  }

  class Slot( val fitness : Fitness
            , val iter : Int
            , val time : Seconds
            ) {
    override def toString: String =
      ("%10d\t"+fitness2Format(fitness)+"\t%10.2f").format(iter, fitness, time)
  }

  class SlotWithFormat( fitness : Fitness
                      , iter : Int
                      , time : Seconds
                      , format : String
                      ) extends Slot(fitness, iter, time) {
    override def toString: String =
      format.format(iter, fitness, time)
  }

  private val slots = new ArrayBuffer[Slot]()

  private def addSlot(slot : Slot): Unit = {
    slots += slot
    if(echo)
      println(slot)
  }

  def bestFitness : Fitness =
    slots.last.fitness

  def timeBestFitness : Seconds =
    slots.last.time max 0.00001

  def register(iter: Int, fitness: Fitness): Unit = {
    val slot = new Slot(fitness, iter, timer.elapsedTime())
    addSlot(slot)
  }

  def register(format: String, iter: Int, fitness: Fitness): Unit = {
    val slot = new SlotWithFormat(fitness, iter, timer.elapsedTime(), format)
    addSlot(slot)
  }

  def print(prefix : String = "", maxTime : Seconds = -1, step : Int = 50, pw : java.io.PrintWriter = new java.io.PrintWriter(System.out), logFormat: String = null): Unit = {
    if(slots.isEmpty) {
      pw.println(prefix+"No solutions!")
      return
    }

    val maxTime1 = if(maxTime<0) slots.last.time + step else maxTime
    val last = slots.last


    pw.println((prefix+"LAST: "+fitness2Format(last.fitness)).format(last.fitness))

    for(mt <- 0 to maxTime1.toInt by step) {
      val xs = slots.takeWhile(_.time <= mt)
      if(xs.nonEmpty) {
        val last = xs.last
        pw.println((prefix+"LAST %d: "+fitness2Format(last.fitness)).format(mt, last.fitness))
      }
    }

    val logFmt = if(logFormat!=null) logFormat else "%d "+fitness2Format(last.fitness)+" %.4f  "
    pw.print(prefix+"LOG: ")
    for(s <- slots)
      pw.print(logFmt.format(s.iter, s.fitness, s.time))
      // pw.print(s.toString)
    pw.println()
    pw.flush()
  }
}
