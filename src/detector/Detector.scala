package detector

import image._
import Helpers._

class Detector(img:Image) {
  val intImg = new IntegralImage(img)
  
  def getFacePositionsAndProbs():List[(Rect, Double)] = ???
  
  def markFaces():Image = {
    val asProb = true
    
    val imgCopy = img.copy()
    var g = imgCopy.g
    for((rect, prob) <- getFacePositionsAndProbs()) {
      if(asProb)
        g.setColor(Color.fromProbToHeat(prob.toFloat).color)
      else
        g.setColor((if(prob > 0.5) Color.red else Color.blue).color)
      
      g.drawRect(rect.x, rect.y, rect.w, rect.h)
    }
    
    imgCopy
  }
}
object Detector {
  val filename = "filename.txt"
  val haars = loadHaarsFromFile(filename)
  
  def loadHaarsFromFile(filename:String):List[Haar] = ???
}
