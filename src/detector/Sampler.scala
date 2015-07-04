/*
 * TODO
 * 
 * Add scaling
 */

package detector

import file._

object Sampler extends App {
  println("sampler starting")
  val startTime = System.currentTimeMillis

//  val file = "res/samples/cropped_face_small.jpg"
  val file = "res/samples/face00010.png"
  
  val detector = new Detector(File.readImage(file))
  val marked = detector.markedImage
  marked.toFile("res/output/markedFaces")
  
  val endTime = System.currentTimeMillis
  println("program finished in "+(endTime-startTime)+" ms")
}
