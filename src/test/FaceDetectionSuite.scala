package test

import scala.util.Random
import org.scalatest.FunSuite
import detector._
import image._
import file._
import Helpers._

class FaceDetectionSuite extends FunSuite {
  //https://youtu.be/Wwn81tVIR10?t=4m50s
  val arrTrue = Array(Array(5,2,5,2),
                      Array(3,6,3,6),
                      Array(5,2,5,2),
                      Array(3,6,3,6))
  val summedArrTrue = Array(Array(5,7,12,14),
                            Array(8,16,24,32),
                            Array(13,23,36,46),
                            Array(16,32,48,64))
  
  test("IntImg - integral image is created correctly") {
    val img = Image.from2dIntArray(arrTrue)
    val intImg = new IntegralImage(img)
    val summedArr = intImg.toArray()
    
    for(i <- 0 to 3; j <- 0 to 3)
      assert(summedArr(i)(j) == summedArrTrue(i)(j))
//    assert(intImg.arr == summedArr)
  }
  
  test("IntImg - integral image sums regions properly") {
    val img = Image.from2dIntArray(arrTrue)
    val intImg = new IntegralImage(img)
    
    assert(intImg.getSum((0, 0, 2, 2)) == 5+2+3+6)
    assert(intImg.getSum((1, 1, 2, 2)) == 6+3+2+5)
    assert(intImg.getSum((0, 0, 4, 4)) == 5+2+5+2+3+6+3+6+5+2+5+2+3+6+3+6)
    assert(intImg.getSum((2, 2, 2, 2)) == 5+2+3+6)
    assert(intImg.getSum((0, 0, 1, 4)) == 5+3+5+3)
    assert(intImg.getSum((0, 0, 4, 1)) == 5+2+5+2)
  }
  
  test("IntImg - integral image gets sub-regions correctly") {
    val img = Image.from2dIntArray(arrTrue)
    val intImg = new IntegralImage(img)
    val summedArr = intImg.toArray()
    
    val rect1 = new Rect(1, 2, 3, 2)
    val subRegion1 = intImg.subRegion(rect1).toArray()
    val subRegion1True = Array(Array(23,36,46),
                               Array(32,48,64))
    for(i <- 0 to subRegion1True.length-1; j <- 0 to subRegion1True(0).length-1)
      assert(subRegion1(i)(j) == subRegion1True(i)(j))
    
  }
  
  test("IntImg - integral image gets sub-regions at bounds correctly") {
    val img = Image.from2dIntArray(arrTrue)
    val intImg = new IntegralImage(img)
    val summedArr = intImg.toArray()
    
    val rect1 = new Rect(2, 2, 2, 2)
    val subRegion1 = intImg.subRegion(rect1).toArray()
    val subRegion1True = Array(Array(36,46),
                               Array(48,64))
    for(i <- 0 to subRegion1True.length-1; j <- 0 to subRegion1True(0).length-1)
      assert(subRegion1(i)(j) == subRegion1True(i)(j))
    
    val rect2 = new Rect(0, 0, 4, 4)
    val subRegion2 = intImg.subRegion(rect2).toArray()
    val subRegion2True = summedArrTrue
    for(i <- 0 to subRegion2True.length-1; j <- 0 to subRegion2True(0).length-1)
      assert(subRegion2(i)(j) == subRegion2True(i)(j))
  }
  
  test("IntImg - haar diffs correctly even when offset") {
    val img = Image.from2dIntArray(arrTrue)
    
    val rect1 = new Rect(1, 1, 3, 3)
    val intImg = new IntegralImage(img).subRegion(rect1)
    
//    (6,3,6),
//    (2,5,2),
//    (6,3,6))
    
    val r1 = new Rect(0, 0, 1, 3) //6+2+6
    val r2 = new Rect(1, 0, 1, 3) //3+5+3
    val haar = new Haar(r1, r2)
    val diff = haar.getDiff(intImg)
    val expected = (6+2+6)-(3+5+3)
    assert(diff == expected, "["+(6+2+6)+" - "+(3+5+3)+"]")
    
  }
  
  test("Haar - haar diffs correctly") {
    //https://youtu.be/Wwn81tVIR10?t=4m50s
    val img = Image.from2dIntArray(arrTrue)
    val intImg = new IntegralImage(img)
    
//  (5,2,5,2),
//  (3,6,3,6),
//  (5,2,5,2),
//  (3,6,3,6))
    
    val r1 = new Rect(0, 0, 1, 3) //5+3+5+2+6+2
    val r2 = new Rect(1, 1, 3, 2) //6+3+6+2+5+2
    val haar = new Haar(r1, r2)
    assert(haar.getDiff(intImg) == (5+3+5+2+6+2)-(6+3+6+2+5+2))
    
  }
  
  test("Haar - serialization matches deserialization") {
    val originalHaar = Haar.random()
    val originalString = originalHaar.serialize()
    val newHaar = Haar.deserialize(originalString)
    val newString = originalHaar.serialize()
    
    assert(originalHaar == newHaar)
    assert(originalString == newString)
  }

  test("Haar - serialization & deserialization is lossless") {
    val originalHaar = Haar.random()
    val originalString = originalHaar.serialize()
    val newHaar = Haar.deserialize(originalString)
    val newString = originalHaar.serialize()
    
    assert(originalHaar.toString == newHaar.toString)
  }
  
  test("Trainer - training should improve accuracy") {
    
    def randImageInDir(dir:String):Image = {
      val files = File.getFilesIn(dir)
      val imageFiles = files.filter { File.isImage }
      val randImageFile = imageFiles((Math.random()*imageFiles.length).toInt)
      File.readImage(randImageFile)
    }
    
    def faceProb(img:Image):Double = {
      val detector = new Detector(img)
      val facePositionsAndProbs = detector.getFacePositionsAndProbs()
      facePositionsAndProbs.map(_._2).sum*1.0/facePositionsAndProbs.length
    }
    
    val faceImg = randImageInDir("res/face_db_3/test/face")
    val faceProb1 = faceProb(faceImg)
    
    val nonFaceImg = randImageInDir("res/face_db_3/test/non-face")
    val nonFaceProb1 = faceProb(nonFaceImg)
    
    Trainer.train()
    
    val faceProb2 = faceProb(faceImg)
    val nonFaceProb2 = faceProb(nonFaceImg)
    
    assert(faceProb2 > faceProb1) //faces are judged more face-like after training
    assert(nonFaceProb2 < nonFaceProb1) //non-faces are judged less face-like after training
    
    assert(faceProb2 > 0.5, "after training, a face should be scored > 0.5 prob")
    assert(nonFaceProb2 < 0.5, "after training, a non-face should be scored < 0.5 prob")
  }
}
