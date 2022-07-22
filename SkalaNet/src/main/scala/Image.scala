package SkalaNet

case class Image private (val label: Int, private val pixels: Array[Array[Int]]):
    def toColumnVector(): Matrix = ???

    override def toString(): String = 
        import math.floorDiv
        val byte2ascii = """ .'`^",:;Il!i><~+_-?][}{1)(|\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"""
        val LEN = byte2ascii.length
        val digit = pixels.map("|" + _.map(pixel => byte2ascii(floorDiv(pixel * LEN, 256))).mkString + "|").mkString("\n")
        val bar = "+" + "-".padTo(28, "-").mkString + "+" // "-".repeat(28) is not supported by Scala Native :(
        Seq(bar, digit, bar).mkString("\n")

object Image:

    private def readBytes(file: String) = 
        import java.nio.file.{Files, Paths}
        Files.readAllBytes(Paths.get(file))

    private def readLabels(labelFile: String): Seq[Int] = readBytes(labelFile).drop(8).map(_.toInt)

    def readImages(imageFile: String, labelFile: String): Seq[Image] = 
        val labels = readLabels(labelFile)
        readBytes(imageFile).drop(16)
                            .map(_.toInt & 255) // convert to unsigned "byte" by masking with 0b11111111
                            .grouped(28 * 28)
                            .map(_.grouped(28).toArray)
                            .zip(labels)
                            .map(p => Image(pixels = p._1, label = p._2))
                            .toSeq
