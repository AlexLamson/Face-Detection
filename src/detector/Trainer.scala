package detector

import file._

object Trainer extends App {
//  for all possible haar-like features (or random, if there is a time constraint)
//  for each positive image in the training set
//    run the haar-like feature on the image
//    add the calculated difference to a positive list
//  for each negative image in the training set
//    run the haar-like feature on the image
//    add the calculated difference to a negative list
//  calculate a threshold such that the number of errors is minimized
//    (also flip the black and white side if necessary)
//  choose the top 1000 features with the lowest error rates (this is the cascade)
}
