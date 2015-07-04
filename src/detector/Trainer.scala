package detector

import file._

object Trainer extends App {
  println("trainer starting")
  val startTime = System.currentTimeMillis
  
  val filename = "features.txt"
  
  val (winWidth, winHeight) = (19, 19)
  
  val haarList = (1 to 10).map{ Haar.random(winWidth, winHeight) }
  
  
}
