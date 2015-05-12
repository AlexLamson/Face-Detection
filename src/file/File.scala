/*
 * File reads and writes files. Also has some directory traversal methods.
 */

/*
 * TODO
 * add comments to all methods
 */

package file

import java.nio.file.Paths
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.io.FileWriter
import scala.io.Source
import java.io.{File => JFile}
import javax.imageio.ImageIO
import java.io.IOException
import scala.util.Random
import scala.collection.JavaConversions._

import image.Image

object File{
  def readLines(filename:String) = Source.fromFile(filename).getLines
  def toFile(filename:String, str:String):Unit = 
    Files.write(Paths.get(filename), bytes(str))
  def appendToFile(filename:String, str:String):Unit = {
    val fw = new FileWriter(filename, true)
    try { fw.write(str) }
    finally fw.close()
  }
  
  def readImage(file:JFile):Image = new Image(ImageIO.read(file))
  def readImage(filename:String):Image = readImage(toJFile(filename))
  def toFile(filename:String, image:Image):Unit = {
    try{
      ImageIO.write(image.img, "png", new JFile(filename+".png"))
    }catch{
      case ioe: IOException => println("there was an IOException")
      case e: Exception => println("there was an Exception")
    }
  }
  
  def getFilesIn(dir:String):List[JFile] = getFilesIn(toJFile(dir))
  def getFilesIn(file:JFile):List[JFile] = {
    if(file.isDirectory)
      file.listFiles.foldLeft(List[JFile]()) { (a:List[JFile], b:JFile) => a ::: getFilesIn(b) }
    else List(file)
  }
  
  def randomFile(dir:String):JFile = randomFile(toJFile(dir))
  def randomFile(dir:JFile):JFile = Random.shuffle( getFilesIn(dir) ).head
  
  def imageIterator(dir:String):Iterator[Image] = imageIterator(toJFile(dir))
  def imageIterator(dir:JFile) = new Iterator[Image] {
    val files = getFilesIn(dir).filter(File.isImage).iterator
    def hasNext = files.hasNext
    def next = readImage(files.next)
  }
  
  def isImage(file:JFile):Boolean = {
    val fileType = Files.probeContentType(file.toPath)
    fileType != null && fileType.startsWith("image")
  }
  
  private def toJFile(filename:String):JFile = new JFile(filename)
  private def bytes(str:String) = str.getBytes(StandardCharsets.UTF_8)
}
