package detector

object Trainer extends App {
//    generate many random haar-like features
//  for each image in the training set
//    for each haar-like feature
//      run the haar-like feature on the image
//      if feature was positive, add 1 to its counter
//  for each haar-like feature
//    feature score = counter / # training images
//    if feature score is < 0.5
//      feature score = 1.0 - feature score
//      flip the black and white sides
//  sort all the haar-like features in descending order by their score
}
