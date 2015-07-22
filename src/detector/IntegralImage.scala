package detector

import image._
import Helpers._

class IntegralImage(var arr:Array[Array[Int]]) {
  def this(img:Image) = this(img.to2dIntArray())
  
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
  def getSum(r:Rect, dx:Int=0, dy:Int=0):Int = {
    val (x, y, w, h) = r
    
    assert(dx+x >= 0, s"x: $x width: $width w: $w dx: $dx")
    assert(dx+x+w <= this.width, s"x: $x width: $width w: $w dx: $dx")
    assert(dy+y >= 0, s"y: $y height: $height h: $h dy: $dy")
    assert(dy+y+h <= this.height, s"y: $y height: $height h: $h dy: $dy")
    
    //AB
    //CD
    val A = topLeft(dx+x, dy+y)
    val B = top(dx+x+w-1, dy+y)
    val C = left(dx+x, dy+y+h-1)
    val D = get(dx+x+w-1, dy+y+h-1)
    
    
    println("summing region: "+r+" => "+(D-C-B+A))
    println("corners: "+(A, B, C, D))
    
    D - C - B + A
  }
  
  def getDiff(haar:Haar, x:Int=0, y:Int=0) = ???
  
  //returns summed area table as an array
  def toArray():Array[Array[Int]] = arr

  override def toString = {
    val rows = for{
      row <- arr
    } yield row.mkString(",")
    rows.mkString("\n")
  }
}
