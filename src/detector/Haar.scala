package detector

import Helpers._

class Haar(val white:Rect, val black:Rect) {
  var threshold = 0
  var lessThan = true
  
  def getDiff(intImg:IntegralImage, dx:Int=0, dy:Int=0):Int = {
//    intImg.getSum(white)-intImg.getSum(black)
    val whiteSum = intImg.getSum(white, dx, dy)
    val blackSum = intImg.getSum(black, dx, dy)
    println("whiteSum: "+whiteSum+" blackSum: "+blackSum)
    whiteSum-blackSum
  }
  
  def isFace() = ???
  
  def serialize():String = white+" "+black+" "+threshold+" "+lessThan
}
object Haar {
  val (width, height) = (19, 19)
  
  def deserialize(s:String):Haar = {
    val terms = s.split(" ")
    assert(terms.length == 4)
    val white:Rect = ???//terms(0)
    val black:Rect = ???//terms(1)
    val threshold:Int = ???//terms(2)
    val lessThan:Boolean = ???//terms(3)
    
    val haar = new Haar(white, black)
    haar.threshold = threshold
    haar.lessThan = lessThan
    
    haar
  }
  
  def random():Haar = {
    //generate random int in the range [min, max]
    def rand(min:Int, max:Int) = (math.random*(max-min)+min).toInt
    
    val minWidth = 4
    val minHeight = 4
    
    val centerX = rand(minWidth/2, width-minWidth/2)
    val centerY = rand(minHeight/2, height-minHeight/2)
    val w = rand(minWidth, math.min(centerX*2, (width-centerX)*2)-1)
    val h = rand(minHeight, math.min(centerY*2, (height-centerY)*2)-1)
    val x = centerX-w/2
    val y = centerY-h/2
    
    val hasHorzStripes = math.random < 0.5
    val hasTwoStripes = math.random < 0.5
    
    (hasTwoStripes, hasHorzStripes) match {
      case (true, true) => new Haar((x, y, w, h/2), (x, y+h/2, w, h/2))
      case (true, false) => new Haar((x, y, w/2, h), (x+w/2, y, w/2, h))
      case (false, true) => new Haar((x, y, w, h), (x, y+h/3, w, h/3))
      case (false, false) => new Haar((x, y, w, h), (x+w/3, y, w/3, h))
    }
  }
}

//class Rect(val x:Int, val y:Int, val w:Int, val h:Int) {
//  lazy val tuple = (x, y, w, h)
//}
object Helpers {
  implicit class RectTuple(t:(Int,Int,Int,Int)) {
    lazy val x = t._1
    lazy val y = t._2
    lazy val w = t._3
    lazy val h = t._4
    
    def serialize():String = ???
  }
  
  type Rect = Tuple4[Int,Int,Int,Int]
  
  object Rect {
    def deserialize(s:String):Rect = ???
  }
}
