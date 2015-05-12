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
    var whiteFirst:Boolean = true){
  
  var threshold = 0.5
  var weight = 0.5
  
  //white area minus black area
  def calcDifference(img:IntegralImage, dx:Int, dy:Int):Int
  def calcDifference(img:IntegralImage):Int = calcDifference(img, 0, 0)
  
  def isFaceFeature(diff:Int):Boolean = diff < threshold
  def isFaceFeature(img:IntegralImage):Boolean = 
    isFaceFeature(calcDifference(img))
  
  def update(faceList:List[Int], nonFaceList:List[Int]):Unit = {
    val (threshold, polarity, percentErrors) = Haar.getThresholdAndPolarityAndError(faceList, nonFaceList)
    
    this.threshold = threshold
    
    if(polarity == -1) whiteFirst = !whiteFirst
    
    weight = 1.0-percentErrors //not totally sure about this part
  }
}
object Haar {
  
  //given diff values (made by a single haar-like feature) for faces and non-faces, 
  //  determine a threshold to separate the two
  //note that if the polarity is -1, then the white and black regions of 
  //  the haar-like feature should be swapped
  //returns (threshold, polarity, percentage of errors)
  def getThresholdAndPolarityAndError(faceList:List[Int], nonFaceList:List[Int]):(Int, Int, Double) = {
    val faceAvg = 1.0*faceList.sum/faceList.length
    val nonFaceAvg = 1.0*nonFaceList.sum/nonFaceList.length
    
    //polarity will be 1 if face values more positive than non-face values on average
    val polarity = if(faceAvg > nonFaceAvg) 1 else -1
    
    def getThreshold(negList:List[Int], posList:List[Int]) = {
      /*
       * Explanation
       * + = face value, - = non-face value
       * - --- - -+-- +- + +++ +++
       * 
       * The idea is that the face and non-face values are clustered in
       * two regions, with some overlap between the two clusters.
       * Each cluster has a "tail" of values that intersect the other cluster.
       * The threshold is the average of the medians of the two tails.
       * 
       */
      (posList.min + negList.max)/2
      
      val posTail = posList.filter( _ < negList.max )
      val posMedian = posTail.sorted.apply(posTail.length/2) //note that sorting will be somewhat slow when scaled up
      
      val negTail = negList.filter( _ > posList.min )
      val negMedian = negTail.sorted.apply(negTail.length/2) //note that sorting will be somewhat slow when scaled up
      
      (posMedian + negMedian)/2
    }
    val threshold = if(polarity == 1) getThreshold(nonFaceList, faceList) else getThreshold(faceList, nonFaceList)
//    println("threshold: "+threshold)
    
    val falseNegatives = faceList.filter(_*polarity < threshold*polarity).length
    val falsePositives = nonFaceList.filter(_*polarity > threshold*polarity).length
    val errors = falseNegatives + falsePositives
    val percentErrors = 1.0*errors/(faceList.length + nonFaceList.length)
//    val nonErrors = faceList.length+nonFaceList.length-errors
//    println("false negatives: "+falseNegatives)
//    println("false positives: "+falsePositives)
//    println("errors: "+errors)
//    println("non-errors: "+nonErrors)
    
    (threshold, polarity, percentErrors)
  }
  
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
