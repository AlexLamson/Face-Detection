package detector

object Trainer extends App {
  
  train()
  
  def train(filename:String="features.txt", 
      facesDir:String="res/face_db_3/train/face", 
      nonFacesDir:String="res/face_db_3/train/non-face") {
    ???
  }
  
  def generateHaars(count:Int):List[Haar] = ???
  
  def updateHaarsAndWeights():List[(Haar, Double)] = ???
  
  def writeHaarsAndWeightsToFile(haars:List[(Haar, Double)]) = ???
}
