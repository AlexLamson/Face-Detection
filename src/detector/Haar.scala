/*
 * TODO
 * 
 * add toFileString/fromFileString methods
 *   Format:
 *   hasHorzStripes hasTwoStripes whiteFirst x y width height threshold
 *   H 2 W 0 0 10 10 0.54124
 * 
 */

package detector

sealed class Rect(val x:Int, val y:Int, val width:Int, val height:Int){
  override def toString = s"($x, $y, $width, $height)"
}

//generic haar-like feature
abstract class Haar(val r:Rect, 
    val hasHorzStripes:Boolean, 
    val hasTwoStripes:Boolean,
    val whiteFirst:Boolean = true){
  
  var threshold = 0.5
  
  //white area minus black area
  def calcDifference(img:IntegralImage, dx:Int, dy:Int):Int
  def calcDifference(img:IntegralImage):Int = calcDifference(img, 0, 0)
  
  def isFaceFeature(diff:Int):Boolean = diff < threshold
  def isFaceFeature(img:IntegralImage):Boolean = 
    isFaceFeature(calcDifference(img))
  
}
object Haar {
  def randomHaar(width:Int, height:Int):Haar = {
    
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

    val r = new Rect(x, y, w, h)
    
    val hasHorzStripes = math.random < 0.5
    val hasTwoStripes = math.random < 0.5
    
    Haar(r, hasHorzStripes, hasTwoStripes)
  }
  
  def apply(r:Rect, hasHorzStripes:Boolean, hasTwoStripes:Boolean) = {
    (hasHorzStripes, hasTwoStripes) match {
      case (true, true) => new HaarH2(r)
      case (false, true) => new HaarV2(r)
      case (true, false) => new HaarH3(r)
      case (false, false) => new HaarV3(r)
    }
  }
}

//haar-like feature with 2 horizontal stripes
class HaarH2(r:Rect) extends Haar(r, true, true) {
  override def calcDifference(img:IntegralImage, dx:Int, dy:Int) = {
    val A = img.sumRegion(r.x+dx, r.y+dy, r.width, r.height/2)
    val B = img.sumRegion(r.x+dx, r.y+r.height/2+dy, r.width, r.height/2)
    val diff = if(whiteFirst) A - B else B - A
    diff
  }
}

//haar-like feature with 2 vertical stripes
class HaarV2(r:Rect) extends Haar(r, false, true) {
  override def calcDifference(img:IntegralImage, dx:Int, dy:Int) = {
    val A = img.sumRegion(r.x+dx, r.y+dy, r.width/2, r.height)
    val B = img.sumRegion(r.x+r.width/2+dx, r.y+dy, r.width/2, r.height)
    val diff = if(whiteFirst) A - B else B - A
    diff
  }
}

//haar-like feature with 3 horizontal stripes
class HaarH3(r:Rect) extends Haar(r, true, false) {
  override def calcDifference(img:IntegralImage, dx:Int, dy:Int) = {
    val A = img.sumRegion(r.x+dx, r.y+dy, r.width, r.height/3) + 
      img.sumRegion(r.x+dx, r.y+2*r.height/3+dy, r.width, r.height/3)
    val B = img.sumRegion(r.x+dx, r.y+r.height/3+dy, r.width, r.height/3)
    val diff = if(whiteFirst) A - B else B - A
    diff
  }
}

//haar-like feature with 3 vertical stripes
class HaarV3(r:Rect) extends Haar(r, false, false) {
  override def calcDifference(img:IntegralImage, dx:Int, dy:Int) = {
    val A = img.sumRegion(r.x+dx, r.y+dy, r.width/3, r.height) + 
      img.sumRegion(r.x+2*r.width/3+dx, r.y+dy, r.width/3, r.height)
    val B = img.sumRegion(r.x+r.width/3+dx, r.y+dy, r.width/3, r.height)
    val diff = if(whiteFirst) A - B else B - A
    diff
  }
}
