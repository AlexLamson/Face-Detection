package detector

import file._

//NOTE: maximum detector window size is 3960 by 3960, 
//given that the brightness differences given by the 
//haar-like features are stored in ints

object Trainer extends App {
  println("trainer starting")
  val startTime = System.currentTimeMillis
  
//  for all possible haar-like features (or random, if there is a time constraint)
  //random haar-like features
  val numFeatures = 1000
  println(s"generating $numFeatures random features")
  val haarList = (1 to numFeatures).map( _ => Haar.randomHaar(19, 19) )
  
  val faceDir = "res/face_db_3/train/face"
  val faceFiles = File.getFilesIn( faceDir ).filter(File.isImage)
  
  val nonFaceDir = "res/face_db_3/train/non-face"
  val nonFaceFiles = File.getFilesIn( nonFaceDir ).filter(File.isImage)
  
  for(feature <- haarList){
      println(feature.toString)
  
//    for each positive image in the training set
//      run the haar-like feature on the image
//      add the calculated difference to a positive list
    val faceDiffs = for{
      faceFile <- faceFiles
      original = File.readImage(faceFile)
      intImg = new IntegralImage(original)
      diff = feature.calcDifference(intImg)
    } yield diff
    
//    for each negative image in the training set
//      run the haar-like feature on the image
//      add the calculated difference to a negative list
    val nonFaceDiffs = for{
      nonFaceFile <- nonFaceFiles
      original = File.readImage(nonFaceFile)
      intImg = new IntegralImage(original)
      diff = feature.calcDifference(intImg)
    } yield diff
    
//    calculate a threshold such that the number of errors is minimized
//      (also flip the black and white side if necessary)
    feature.update(faceDiffs, nonFaceDiffs)
    
    println(feature.toString)
    
    //save the calculated feature to a file
    File.appendToFile("features.txt", feature.toFileString+"\n")
  }
  
//  choose the top 1000 features with the lowest error rates (this is the cascade)
  val sortedFeatures = haarList.sortBy { x => 1.0-x.weight }.filter(_.weight > 0.5)
  println("-"*20)
  println("sorted list of features")
  println("-"*20)
  sortedFeatures.foreach { println }
  
  val endTime = System.currentTimeMillis
  println("program finished in "+(endTime-startTime)+" ms")
}
