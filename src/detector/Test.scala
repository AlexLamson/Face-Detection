package detector

import org.scalatest.FunSuite
import image._
import file._

class FaceDetectionSuite extends FunSuite {

  test("integral image should sum numbers properly") {
    //https://youtu.be/Wwn81tVIR10?t=4m50s
    val arr = Array(Array(5,2,5,2),
      Array(3,6,3,6),
      Array(5,2,5,2),
      Array(3,6,3,6))
    
    val img = Image.from2dIntArray(arr)
    
    val intImg = new IntegralImage(img)
    
    val summedArr = Array(Array(5,7,12,14),
      Array(8,16,24,32),
      Array(13,23,36,46),
      Array(16,32,48,64))
    
    for(i <- 0 to 3)
      for(j <- 0 to 3)
        assert(intImg.arr(i)(j) == summedArr(i)(j))
//    assert(intImg.arr == summedArr)
  }
}
