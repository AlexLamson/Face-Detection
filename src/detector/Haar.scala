package detector

//generic haar-like feature
abstract class Haar(val r:Rect, 
    val hasHorzStripes:Boolean, 
    val hasTwoStripes:Boolean,
    var whiteFirst:Boolean = true,
    var threshold:Int = 0,
    var weight:Double = 0.5){
  
  def polarity = if(whiteFirst) 1 else -1
  
  
  //white area minus black area
  def calcDifference(img:IntegralImage, dx:Int = 0, dy:Int = 0):Int
  
  def isFaceFeature(diff:Int):Boolean = diff*polarity < threshold*polarity
  def isFaceFeature(img:IntegralImage, dx:Int = 0, dy:Int = 0):Boolean = 
    isFaceFeature(calcDifference(img, dx, dy))
  
  def update(faceList:List[Int], nonFaceList:List[Int]) {
    val (threshold, polarity, percentErrors) = Haar.getThresholdAndPolarityAndError(faceList, nonFaceList)
    
    this.threshold = threshold
    
    if(polarity == -1) whiteFirst = !whiteFirst
    
    weight = 1.0-percentErrors //not totally sure about this part
  }
  
  def toFileString:String = {
    val dir = if(hasHorzStripes) "H" else "V"
    val num = if(hasTwoStripes) "2" else "3"
    val white = if(whiteFirst) "W" else "B"
    val (x, y, w, h) = (r.x, r.y, r.width, r.height)
    def f(i:Double) = "%2.2f" format i
    s"$dir $num $white $x $y $w $h $threshold ${f(weight)}"
  }
  
  override def toString = {
    val dir = if(hasHorzStripes) "H" else "V"
    val num = if(hasTwoStripes) "2" else "3"
    val white = if(whiteFirst) "W" else "B"
    val rect = r.toString
    def f(i:Double) = "%2.2f" format i
    s"$dir$num$white$rect t:$threshold w:${f(weight)}"
  }
  
  override def equals(that:Any) = {
    that match {
      case that:Haar => that.r==r &&
        that.hasHorzStripes==hasHorzStripes &&
        that.hasTwoStripes==hasTwoStripes &&
        that.whiteFirst==whiteFirst
      case _ => false
    }
  }
}
object Haar {
  
  def fromFileString(fileString:String):Haar = {
    
    val vars = fileString.split(" ")
    val (dir, num, white, x, y, w, h, threshold, weight) = 
      (vars(0), vars(1), vars(2), vars(3), vars(4), 
          vars(5), vars(6), vars(7), vars(8))
    val rect = new Rect(x.toInt, y.toInt, w.toInt, h.toInt)
    Haar(rect, dir=="H", num=="2", white=="W",
        threshold.toInt, weight.toDouble)
  }
  
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
  
  def apply(r:Rect, 
    hasHorzStripes:Boolean, 
    hasTwoStripes:Boolean,
    whiteFirst:Boolean = true,
    threshold:Int = 0,
    weight:Double = 0.5):Haar = {
    (hasHorzStripes, hasTwoStripes) match {
      case (true, true) => new HaarH2(r, whiteFirst, threshold, weight)
      case (false, true) => new HaarV2(r, whiteFirst, threshold, weight)
      case (true, false) => new HaarH3(r, whiteFirst, threshold, weight)
      case (false, false) => new HaarV3(r, whiteFirst, threshold, weight)
    }
  }
}

sealed class Rect(val x:Int, val y:Int, val width:Int, val height:Int){
  override def toString = s"($x, $y, $width, $height)"
  override def equals(that:Any) = {
    that match {
      case that:Rect => that.x==x && that.y==y &&
        that.width==width && that.height==height
      case _ => false
    }
  }
}
