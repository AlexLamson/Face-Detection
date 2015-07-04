package detector

class Haar(val white:Rect, val black:Rect) {
  def this(rects:(Rect, Rect)) = this(rects._1, rects._2)
  
  var whiteCoefficent = 1.0
  var blackCoefficent = -1.0
  
  var threshold = 0
  
  var positiveDiffs = List[Int]()
  var negativeDiffs = List[Int]()
  
  def getDiff(intImg:IntegralImage) = {
    val whiteSum = intImg.sumRegion(white.x, white.y, white.w, white.h)
    val blackSum = intImg.sumRegion(black.x, black.y, black.w, black.h)
    
    (whiteCoefficent*whiteSum + blackCoefficent*blackSum).toInt
  }
  
  def hasFeature(intImg:IntegralImage) = {
    getDiff(intImg) < threshold
  }
  
  override def toString():String = {
    white.toString()+whiteCoefficent+" "+
      blackCoefficent+" "+black.toString()+
      " "+threshold
  }
  
  def addPositiveDiff(intImg:IntegralImage) {
    positiveDiffs = getDiff(intImg) :: positiveDiffs
  }
  
  def addNegativeDiff(intImg:IntegralImage) {
    positiveDiffs = getDiff(intImg) :: positiveDiffs
  }
  
  def adjustThreshold() {
    val allDiffs = positiveDiffs ::: negativeDiffs
    val thresholdsAndErrors = for {
      possibleThreshold <- allDiffs
      posErrs = positiveDiffs.count { _ >= possibleThreshold }
      negErrs = negativeDiffs.count { _ < possibleThreshold }
      err = posErrs + negErrs
    } yield (possibleThreshold, err)
    
    threshold = thresholdsAndErrors.minBy( _._2 )._1
  }
}
object Haar {
  def random(width:Int, height:Int) = {
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
    
    (hasHorzStripes, hasTwoStripes) match {
      case (true, true) => ???
      case (true, false) => ???
      case (false, true) => ???
      case (false, false) => ???
    }
  }
  
  def fromTypeNum(r:Rect, i:Int):Haar = {
    i match {
      case 0 => new HaarH2(r)
      case 1 => new HaarV2(r)
      case 2 => new HaarH3(r)
      case 3 => new HaarV3(r)
    }
  }
  
  def fromString(s:String):Haar = {
    val nums = s.split(" ").map(_.toInt)
    
    val white = new Rect(nums(0), nums(1), nums(2), nums(3))
    val whiteCoefficent = nums(3)
    
    val black = new Rect(nums(4), nums(5), nums(6), nums(7))
    val blackCoefficent = nums(8)
    
    val threshold = nums(9)
    
    val h = new Haar(white, black)
    h.whiteCoefficent = whiteCoefficent
    h.blackCoefficent = blackCoefficent
    h.threshold = threshold
    
    h
  }
}

class Rect(val x:Int, val y:Int, val w:Int, val h:Int) {
  override def toString() = x+" "+y+" "+w+" "+h
}

class HaarH2(r:Rect) extends Haar(HaarTypes.makeH2(r))
class HaarV2(r:Rect) extends Haar(HaarTypes.makeV2(r))
class HaarH3(r:Rect) extends Haar(HaarTypes.makeH3(r))
class HaarV3(r:Rect) extends Haar(HaarTypes.makeV3(r))

object HaarTypes {
  def makeH2(r:Rect):(Rect, Rect) = {
    val white = new Rect(r.x, r.y, r.w, r.h/2)
    val black = new Rect(r.x, r.y+r.h/2, r.w, r.h/2)
    (white, black)
  }
  def makeV2(r:Rect):(Rect, Rect) = {
    val white = new Rect(r.x, r.y, r.w/2, r.h)
    val black = new Rect(r.x+r.w/2, r.y, r.w/2, r.h)
    (white, black)
  }
  def makeH3(r:Rect):(Rect, Rect) = {
    val white = new Rect(r.x, r.y, r.w, r.h)
    val black = new Rect(r.x, r.y+r.h/3, r.w, r.h/3)
    (white, black)
  }
  def makeV3(r:Rect):(Rect, Rect) = {
    val white = new Rect(r.x, r.y, r.w, r.h)
    val black = new Rect(r.x+r.w/3, r.y, r.w/3, r.h)
    (white, black)
  }
}
