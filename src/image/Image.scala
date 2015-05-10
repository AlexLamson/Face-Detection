/*
 * Represents an image. Has methods to manipulate image to some degree.
 */

/*
 * TODO
 * add comments to object methods
 */

import java.awt.image.{BufferedImage => BImage}
import java.awt.{Color => JColor}
import scala.collection.JavaConversions._

class Image(val img:BImage) extends Iterable[Color]{
  def this(w:Int, h:Int) = this(new BImage(w, h, BImage.TYPE_INT_RGB))
  def this(s:String) = this(Image.fromFile(s).img)
  
  val width = img.getWidth
  val height = img.getHeight
  
  //returns colors at given position
  def apply(x:Int, y:Int):Color = get(x, y)
  def get(x:Int, y:Int):Color = 
    new Color(img.getRGB( mod(x, width), mod(y, height) ))
  private def mod(a:Int, b:Int) = { val c = a % b; if(c < 0) c + b else c }
  
  //change a single pixel in the image
  //usage syntax: someImage(someX, someY) = someColor
  //creates new image, try to use recolor instead
  def update(x:Int, y:Int, color:Color):Image = update(x, y, color.rgb)
  def update(x:Int, y:Int, rgb:Int):Image = {
    val copyImg = copyBImage
    copyImg.setRGB(x, y, rgb)
    new Image(copyImg)
  }
  
  //apply color change to all pixels and return the resulting image
  def recolor(f: Color => Color):Image = recolor((_:Int, _:Int, c:Color) => f(c))
  def recolor(f: (Int, Int, Color) => Color):Image = {
    var newImg = new BImage(width, height, BImage.TYPE_INT_RGB)
    for(y <- 0 to height-1; x <- 0 to width-1)
      newImg.setRGB(x, y, f(x, y, get(x, y)).rgb)
    new Image(newImg)
  }
  
  //return list of colors that return true when put through given function
  def filter(f: (Int, Int, Color) => Boolean):List[Color] = {
    val colors = for{
      y <- 0 to height-1
      x <- 0 to width-1
      c = get(x, y)
      if(f(x, y, c))
    } yield c
    colors.toList
  }
  
  //get the position of the first pixel with the given color
  def find(c:Color):Option[(Int, Int)] = {
    val pixel = iteratorWithPos.find{ p => (p._3 == c) }
    pixel match{
      case None => None
      case Some((x, y, _)) => Some(x,y)
    }
  }
  
  //iterate through all the colors in the image
  def iterator = new Iterator[Color] {
    var (x, y) = (0, 0)
    def hasNext = x < width-1 || y < height-1
    def next = {
      val color = get(x,y)
      x = x + 1
      if(x == width){ x = 0; y = y + 1 }
      color
    }
  }
  
  //iterate through all the pixels (position and color) in the image
  def iteratorWithPos = new Iterator[(Int, Int, Color)] {
    var (x, y) = (0, 0)
    def hasNext = x < width-1 || y < height-1
    def next = {
      val color = get(x,y)
      x = x + 1
      if(x == width){ x = 0; y = y + 1 }
      (x, y, color)
    }
  }
  
  //return the brightest pixel
  def brightest:Color = 
    iterator.foldLeft(Color.black)( (a:Color, b:Color) => Color.chooseBrighter(a, b) )
  
  //make all 3 rgb components equal for all pixels
  def toGrayscale:Image = {
    var newImg = copyBImage
    for(y <- 0 to height-1; x <- 0 to width-1)
      newImg.setRGB(x, y, Color.fromProbToGray(get(x, y).v).rgb)
    new Image(newImg)
  }
  
  //average all the colors in the image
  def average:Color = Color.average(iterator.toList)
  
  //apply gaussian blur to image using given kernel size
  def blur(kSize:Int) = {
    var newImg = copyBImage
    
    for(y <- 0 to height-1; x <- 0 to width-1){
      val kColors = for{
        y1 <- y-kSize/2 to y+kSize/2
        x1 <- x-kSize/2 to x+kSize/2
        y2 = mod(y1, height)
        x2 = mod(x1, width)
      } yield newImg.getRGB(x2, y2)
      
      val avgColor = Color.average(kColors.map(new Color(_)).toList)
      
      newImg.setRGB(x, y, avgColor.rgb)
    }
    new Image(newImg)
  }
  
  //set width and height (only use to shrink image)
  def deRes(w:Int, h:Int):Image = {
    var newImg = new BImage(w, h, BImage.TYPE_INT_RGB)
    for(y <- 0 to h-1; x <- 0 to w-1)
      newImg.setRGB(x, y, get(x*width/w, y*height/h).rgb)
    new Image(newImg)
  }
  
  //shrink image so it fits in given rect while keeping aspect ratio
  def constrain(maxWidth:Int, maxHeight:Int):Image = {
    val w = math.min(width, maxWidth)
    val newHeight = 1d*w/width*height
    val hFinal = math.min(newHeight, maxHeight)
    val wFinal = 1d*hFinal/height*width
    
    deRes(wFinal.toInt, hFinal.toInt)
  }
  
  //turn rectangle of pixels in image into a new image
  def subImage(x:Int, y:Int, w:Int, h:Int) = new Image(img.getSubimage(0, 0, w, h))
  
  //copy the whole image, pixel by pixel
  def copy:Image = new Image(copyBImage)
  private def copyBImage = {
    var newImg = new BImage(width, height, BImage.TYPE_INT_RGB)
    for(y <- 0 to height-1; x <- 0 to width-1)
      newImg.setRGB(x, y, get(x,y).rgb)
    newImg
  }
  
  def g = img.getGraphics
  
  //change every color into an int in range [0,255]
  def to2dIntArray() = {
    def getRow(y:Int) = (for{ x <- 0 to width-1 } yield (get(x, y).v*255).toInt).toArray
    (for{ y <- 0 to height-1 } yield getRow(y)).toArray
  }
  
  //change every color into an float in range [0,1]
  def to2dFloatArray():Array[Array[Float]] = {
    def getRow(y:Int) = (for{ x <- 0 to width-1 } yield get(x, y).v).toArray
    (for{ y <- 0 to height-1 } yield getRow(y)).toArray
  }
  
  //write the image to a file
  def toFile(filename:String) = File.toFile(filename, this)
  
  override def toString = "Image("+width+", "+height+")"
}
object Image{
  def generate(width:Int, height:Int, f: (Int, Int) => Color):Image = 
    new Image(width, height).recolor((x:Int, y:Int, _:Color) => f(x,y))
  
  def fromFile(filename:String) = File.readImage(filename)
  
  def from2dIntArray(arr:Array[Array[Int]]):Image = {
    val height = arr.length
    val width = arr(0).length
    
    var newImg = new BImage(width, height, BImage.TYPE_INT_RGB)
    for(y <- 0 to height-1; x <- 0 to width-1){
      val num = arr(y)(x)
      newImg.setRGB(x, y, new Color(num, num, num).rgb)
    }
    new Image(newImg)
  }
  
  def from2dFloatArray(arr:Array[Array[Float]]):Image = {
    val width = arr(0).length
    val height = arr.length
    var newImg = new BImage(width, height, BImage.TYPE_INT_RGB);
    
    for(y <- 0 to height-1; x <- 0 to width-1)
      newImg.setRGB(x, y, Color.fromProbToGray(arr(y)(x)).rgb)
    
    new Image(newImg)
  }
  
  def fromColorList(colors:List[Color]):Image = {
    val width = Math.sqrt(colors.length).ceil.toInt
    val height = width
    
    var newImg = new BImage(width, height, BImage.TYPE_INT_RGB);
    val iterator = colors.iterator
    
    for(y <- 0 to height-1; x <- 0 to width-1){
        if(iterator.hasNext) newImg.setRGB(x, y, iterator.next.rgb)
        else newImg.setRGB(x, y, Color.white.rgb)
    }
    
    new Image(newImg)
  }
}
