package detector

import image.Image

class IntegralImage(var arr:Array[Array[Int]]) {
  def this(img:Image) = this(img.to2dIntArray)
  
  val width = arr(0).length
  val height = arr.length
  
  makeTable
  
  //convert the given array into a summed area table
  private def makeTable {
    for(y <- 0 to height-1; x <- 0 to width-1)
      arr(y)(x) = arr(y)(x) + top(x,y) + left(x,y) - topLeft(x,y)
  }
  
  //get value of cell to the left of given cell
  private def left(x:Int, y:Int) = if(x == 0) 0 else get(x-1, y)
  
  //get value of cell above of given cell
  private def top(x:Int, y:Int) = if(y == 0) 0 else get(x, y-1)
  
  //get value of cell to the upper left of given cell
  private def topLeft(x:Int, y:Int) = if(x == 0 || y == 0) 0 else get(x-1, y-1)
  
  //get value of cell at given position
  def apply(x:Int, y:Int) = get(x, y)
  def get(x:Int, y:Int) = {
    assert(x >= 0, s"x: $x width: $width")
    assert(x < width, s"x: $x width: $width")
    assert(y >= 0, s"y: $y height: $height")
    assert(y < height, s"y: $y height: $height")
    arr(y)(x)
  }
  
  //give the sum of all the cells in the given rectangle
  def sumRegion(x:Int, y:Int, width:Int, height:Int) = {
    
    val (w,h) = (this.width, this.height)
    
    assert(x >= 0, s"x: $x width: $width w:$w")
    assert(x+width < this.width, s"x: $x width: $width w:$w")
    assert(y >= 0, s"y: $y height: $height h:$h")
    assert(y+height < this.height, s"y: $y height: $height h:$h")
    
    //AB
    //CD
    val A = topLeft(x, y)
    val B = top(x+width, y)
    val C = left(x, y+height)
    val D = arr(y)(x)
    
    D - C - B + A
  }
  
  override def toString = {
    val rows = for{
      row <- arr
    } yield row.mkString(",")
    rows.mkString("\n")
  } 
}
