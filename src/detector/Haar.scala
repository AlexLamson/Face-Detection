package detector

import Helpers._

class Haar(val white:Rect, val black:Rect) {
  var threshold = 0
  var lessThan = true
  
  def getDiff(intImg:IntegralImage):Int = ???
  
  def serialize():String = ???
}
object Haar {
  val (w, h) = (19, 19)
  
  def deserialize(s:String):Haar = ???
  
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
  }
  
  type Rect = Tuple4[Int,Int,Int,Int]
}
