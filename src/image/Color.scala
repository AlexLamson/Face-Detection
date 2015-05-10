/*
 * Represents a color. Has methods to manipulate color to some degree.
 */

/*
 * TODO
 * add comments to all methods
 */

import java.awt.{Color => JColor}

class Color(val color:JColor){
  def this(r:Int, g:Int, b:Int) = this(new JColor(r, g, b))
  def this(c:(Int,Int,Int)) = this(new JColor(c._1, c._2, c._3))
  def this(rgb:Int) = this(new JColor(rgb))
  def this() = this(new JColor(0))
  
  lazy val r = color.getRed
  lazy val g = color.getGreen
  lazy val b = color.getBlue
  lazy val rgb = color.getRGB
  
  lazy val y = (0.257*r + 0.504*g + 0.098*b + 16).toInt
  lazy val cb = (-0.148*r - 0.291*g + 0.439*b + 128).toInt
  lazy val cr = (0.439*r - 0.368*g - 0.071*b + 128).toInt
  
  lazy val hsv = JColor.RGBtoHSB(r, g, b, null)
  lazy val (h, s, v) = (hsv(0), hsv(1), hsv(2))
  
  def toRgb:(Int,Int,Int) = (r, g, b)
  def toHsv:(Float,Float,Float) = (h, s, v)
  def toYcbcr:(Int,Int,Int) = (y, cb, cr)
  
  def distanceTo(c:Color):Int = {
    def sq(i:Int) = i*i
    sq(r-c.r)+sq(g-c.g)+sq(b-c.b)
  }
  
  def closest(points:List[Color]):Option[Color] = {
    if(points.isEmpty) return None
    def closer(c1:Color, c2:Color):Color = if(distanceTo(c1) < distanceTo(c2)) c1 else c2
    Some(points.foldLeft(points(0))( (a,b) => closer(a,b) ))
  }
  
  def +(c:Color):Color = new Color(constrain(r+c.r, g+c.g, b+c.b))
  def -(c:Color):Color = new Color(constrain(r-c.r, g-c.g, b-c.b))
  
  //forces a, b & c each within the range [0,255]
  private def constrain(a:Int, b:Int, c:Int) = {
    def bound(i:Int) = math.min( math.max(0,i) ,255)
    (bound(a), bound(b), bound(c))
  }
  
  override def toString = "("+r+", "+g+", "+b+")"
  
  override def equals(that:Any):Boolean = {
    that match {
      case c:Color => (r == c.r && g == c.g && b == c.b)
      case _ => false
    }
  }
}
object Color{
  def fromRgb(r:Int, g:Int, b:Int):Color = new Color(r, g, b)
  def fromHsv(h:Float, s:Float, v:Float):Color = new Color(JColor.HSBtoRGB(h, s, v))
  def fromYcbcr(y:Int, cb:Int, cr:Int):Color = {
    val red = 1.164*(y - 16) + 1.596*(cr - 128)
    val green = 1.164*(y - 16) - 0.813*(cr - 128) - 0.392*(cb - 128)
    val blue = 1.164*(y - 16) + 2.017*(cb - 128)
    def bound(i:Double) = (math.min(255, math.max(0, i))).toInt
    new Color(bound(red), bound(green), bound(blue))
  }
  
  def fromProbToGray(p:Float):Color = {
    assert(p >= 0 && p <= 1, "p out of bounds: "+p)
    new Color((p*255).toInt, (p*255).toInt, (p*255).toInt)
  }
  def fromProbToBlackAndWhite(p:Float, threshold:Float):Color = 
    if(p < threshold) black else white
  def fromProbToHeat(p:Float):Color = {
    assert(p >= 0 && p <= 1, "p out of bounds: "+p)
    val (hue, sat, bri) = ((1f-p)*260/360, 1, 1)
    fromHsv(hue, sat, bri)
  }
  
  def average(colors:List[Color]):Color = {
    val rAvg = colors.map(_.r).sum / colors.length
    val gAvg = colors.map(_.g).sum / colors.length
    val bAvg = colors.map(_.b).sum / colors.length
    new Color(rAvg, gAvg, bAvg)
  }
  
  def chooseBrighter(c1:Color, c2:Color) = 
    if(white.distanceTo(c1) < white.distanceTo(c2)) c1 else c2
  def getBrightest(colors:List[Color]):Option[Color] = {
    if(colors.isEmpty) return None
    Some(colors.foldLeft(colors(0))( (a,b) => chooseBrighter(a,b) ))
  }
  
  def random:Color = randomRgb
  def randomRgb:Color = {
    def rand = (math.random*255).toInt
    new Color(rand, rand, rand)
  }
  def randomHsv:Color = {
    def rand = (math.random).toFloat
    Color.fromHsv(rand, rand*0.2f+0.8f, rand*0.2f+0.8f)
  }
  
  private def c(c:JColor) = new Color(c)
  val black = c(JColor.black)
  val blue = c(JColor.blue)
  val cyan = c(JColor.cyan)
  val darkGray = c(JColor.darkGray)
  val gray = c(JColor.gray)
  val green = c(JColor.green)
  val lightGray = c(JColor.lightGray)
  val magenta = c(JColor.magenta)
  val orange = c(JColor.orange)
  val pink = c(JColor.pink)
  val red = c(JColor.red)
  val white = c(JColor.white)
  val yellow = c(JColor.yellow)
}

