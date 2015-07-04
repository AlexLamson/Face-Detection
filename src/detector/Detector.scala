package detector

import image._

class Detector(val img:Image) {
  val intImg = new IntegralImage(img)
  
  val filename = "features.txt"
  
  def markedImage():Image = ???
}
