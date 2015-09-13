package detector

object Optimize {
  def getThresholdAndLessThanCheap(pos:List[Int], neg:List[Int]):(Int, Boolean) = {
    val posAvg = pos.sum/pos.length
    val negAvg = neg.sum/neg.length
    val threshold = (posAvg + negAvg) / 2
    val isLessThan = (posAvg < negAvg)
    (threshold, isLessThan)
  }
  
  def getThresholdAndLessThanHQ(pos:List[Int], neg:List[Int]):(Int, Boolean) = {
    
    //where x is the proposed threshold and
    //isLessThan is true if positives are generally less than that threshold
    def cost(x:Int, isLessThan:Boolean):Int = {
      val truePositives = pos.count { if(isLessThan) _ < x else _ >= x }
      val falsePositives = pos.length-truePositives
      
      val trueNegatives = neg.count { if(isLessThan) _ >= x else _ < x }
      val falseNegatives = neg.length-trueNegatives
      
      falsePositives + falseNegatives
    }
    
    val allNums = (pos.union(neg))
    val bestTrue = allNums.minBy { cost(_, true) }
    val bestFalse = allNums.minBy { cost(_, false) }
    
    if(cost(bestTrue, true) < cost(bestFalse, false)) (bestTrue, true)
    else (bestFalse, false)
  }
  
  def main(args:Array[String]) {
    
    import scala.util.{Random => R}
    def rand(avg:Double, stdDev:Double) = R.nextGaussian() * stdDev + avg
    
    val pos = (1 to 100).map{_ => rand(40, 10).toInt}.toList
    val neg = (1 to 100).map{_ => rand(70, 20).toInt}.toList
    
//    val pos = List(1, 2, 3, 4, 5, 6, 7, 8).map { _+3 }
//    val neg = pos.map { _-3 }
    
//    println("pos: "+pos)
//    println("neg: "+pos)
    
    val (threshold, lessThan) = getThresholdAndLessThanHQ(pos, neg)
    println(threshold)
    println(lessThan)
    
    val allNums = pos.union(neg)
    for(x <- allNums.min to allNums.max) {
      if(pos.contains(x)) print("+ ")
      else print("  ")
    }
    println()
    for(x <- allNums.min to allNums.max) {
      if(neg.contains(x)) print("- ")
      else print("  ")
    }
    println()
    for(x <- allNums.min to allNums.max) {
      if(x == threshold) {
        if(lessThan) print("< ")
        else print("> ")
      }
      else print("  ")
    }
    println()
    val (thresholdA, lessThanA) = getThresholdAndLessThanCheap(pos, neg)
    for(x <- allNums.min to allNums.max) {
      if(x == thresholdA) {
        if(lessThanA) print("< ")
        else print("> ")
      }
      else print("  ")
    }
    println()
  }
}
