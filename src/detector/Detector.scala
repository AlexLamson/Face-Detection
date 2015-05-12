package detector

import image._
import file._

class Detector(img:Image) {
  def markFaces():Image = {
    ???
  }
}
object Detector{
  val haarList = File.readLines("features.txt").map(Haar.fromFileString).toList
  val sortedFeatures = haarList.sortBy { x => 1.0-x.weight }
  
  def isFace(original:Image) = {
    val intImg = new IntegralImage(original)
    val isFace = sortedFeatures.forall { _.isFaceFeature(intImg) }
  }
}
//  for each scale (multiply by 1.25 at each step)
//    for each block of pixels in the image
//      if all haar-like features are positive, mark the block
//      otherwise move to the next block
