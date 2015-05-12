package detector

//haar-like feature with 2 horizontal stripes
class HaarH2(r:Rect,
    whiteFirst:Boolean = true, threshold:Int = 0, weight:Double = 0.5)
    extends Haar(r, true, true, whiteFirst, threshold, weight) {
  override def calcDifference(img:IntegralImage, dx:Int, dy:Int) = {
    val A = img.sumRegion(r.x+dx, r.y+dy, r.width, r.height/2)
    val B = img.sumRegion(r.x+dx, r.y+r.height/2+dy, r.width, r.height/2)
    val diff = if(whiteFirst) A - B else B - A
    diff
  }
}

//haar-like feature with 2 vertical stripes
class HaarV2(r:Rect,
    whiteFirst:Boolean = true, threshold:Int = 0, weight:Double = 0.5)
    extends Haar(r, false, true, whiteFirst, threshold, weight) {
  override def calcDifference(img:IntegralImage, dx:Int, dy:Int) = {
    val A = img.sumRegion(r.x+dx, r.y+dy, r.width/2, r.height)
    val B = img.sumRegion(r.x+r.width/2+dx, r.y+dy, r.width/2, r.height)
    val diff = if(whiteFirst) A - B else B - A
    diff
  }
}

//haar-like feature with 3 horizontal stripes
class HaarH3(r:Rect,
    whiteFirst:Boolean = true, threshold:Int = 0, weight:Double = 0.5)
    extends Haar(r, true, false, whiteFirst, threshold, weight) {
  override def calcDifference(img:IntegralImage, dx:Int, dy:Int) = {
    val A = img.sumRegion(r.x+dx, r.y+dy, r.width, r.height/3) + 
      img.sumRegion(r.x+dx, r.y+2*r.height/3+dy, r.width, r.height/3)
    val B = img.sumRegion(r.x+dx, r.y+r.height/3+dy, r.width, r.height/3)
    val diff = if(whiteFirst) A - B else B - A
    diff
  }
}

//haar-like feature with 3 vertical stripes
class HaarV3(r:Rect,
    whiteFirst:Boolean = true, threshold:Int = 0, weight:Double = 0.5)
    extends Haar(r, false, false, whiteFirst, threshold, weight) {
  override def calcDifference(img:IntegralImage, dx:Int, dy:Int) = {
    val A = img.sumRegion(r.x+dx, r.y+dy, r.width/3, r.height) + 
      img.sumRegion(r.x+2*r.width/3+dx, r.y+dy, r.width/3, r.height)
    val B = img.sumRegion(r.x+r.width/3+dx, r.y+dy, r.width/3, r.height)
    val diff = if(whiteFirst) A - B else B - A
    diff
  }
}
