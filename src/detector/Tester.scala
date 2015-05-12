package detector

import file._

//verify reliability of haar cascade implementation
object Tester extends App {
  println("trainer starting")
  val startTime = System.currentTimeMillis
  
  //load all the haar-like features from a file
  val haarList = File.readLines("features.txt").map(Haar.fromFileString).toList
  
  //sort them so the most reliable is first
  val sortedFeatures = haarList.sortBy { x => 1.0-x.weight }.filter(_.weight > 0.5)
//  println("-"*20)
//  println("sorted list of features")
//  println("-"*20)
//  sortedFeatures.foreach { println }
  
  println("gathering test face images")
  val faceDir = "res/face_db_3/test/face"
  val faceFiles = File.getFilesIn( faceDir ).filter(File.isImage)
  
  println("gathering test non-face images")
  val nonFaceDir = "res/face_db_3/test/non-face"
  val nonFaceFiles = File.getFilesIn( nonFaceDir ).filter(File.isImage)
  
  /////////////////////////////////////////////////
  
  def formatPercent(index:Int, length:Int) =
    ("%2.4f" format (1.0*index/length*100))+"%"
  def printPercent(index:Int, length:Int) =
    if(index % 1000 == 0) println( formatPercent(index, length) )
  
  println("testing faces")
  val faceResults = for{
    (faceFile, i) <- faceFiles.zipWithIndex
    original = File.readImage(faceFile)
    intImg = new IntegralImage(original)
    isFace = sortedFeatures.forall { _.isFaceFeature(intImg) }
    _=printPercent(i, faceFiles.length)
  } yield isFace
  
  println("testing non-faces")
  val nonFaceResults = for{
    (nonFaceFile, i) <- nonFaceFiles.zipWithIndex
    original = File.readImage(nonFaceFile)
    intImg = new IntegralImage(original)
    isFace = sortedFeatures.forall { _.isFaceFeature(intImg) }
    _=printPercent(i, nonFaceFiles.length)
  } yield isFace
  
  
  val numFiles = faceFiles.length + nonFaceFiles.length
  val falseNegatives = faceResults.count { _ == false }
  val falsePositives = nonFaceResults.count { _ == true }
  val errors = falseNegatives + falsePositives
  val errorPercent = 1.0*errors/numFiles
  val nonErrors = numFiles - errors
  
  println("-"*20)
  println("accuracy: "+ formatPercent(nonErrors, numFiles))
  println("non-errors: "+nonErrors)
  println("errors: "+errors)
  println("false negatives: "+falseNegatives)
  println("false positives: "+falsePositives)
  
  /////////////////////////////////////////////////
  
  val endTime = System.currentTimeMillis
  println("program finished in "+(endTime-startTime)+" ms")
  
//  for each block of pixels in the image
//    for each haar-like feature (in the now sorted order)
//      run the haar-like feature on the image
//      if it is positive and the last feature, mark the block
//      if it is positive and not the last feature, continue
//      if it is negative, move to the next block
}
