package detector

object TestAccuracy extends App {
  val filename = "filename.txt"
  val haars = loadHaarsAndWeights(filename)
  
  def loadHaarsAndWeights(filename:String):List[(Haar, Double)] = ???

}