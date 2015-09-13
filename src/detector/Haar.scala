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
  val (w, h) = (19, 19)
  
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
  
  def random():Haar = ???
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
