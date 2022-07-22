package SkalaNet

case class Image private (private val pixels: Array[Array[Int]]):
    def toColumnVector(): Matrix = ???

    override def toString(): String = 
        import math.floorDiv
        val byte2ascii = """$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\|()1{}[]?-_+~<>i!lI;:,"^`'. """
        val LEN = byte2ascii.length
        val digit = pixels.map("|" + _.map(pixel => byte2ascii(LEN - 1 - floorDiv(pixel * LEN, 256))).mkString + "|").mkString("\n")
        val bar = "+" + "-".padTo(28, "-").mkString + "+" // "-".repeat(28) is not supported by Scala Native :(
        Seq(bar, digit, bar).mkString("\n")

object Image:
    def readImages(file: String): Seq[Image] = 
        import java.nio.file.{Files, Paths}
        Files.readAllBytes(Paths.get(file))
             .drop(16)
             .map(_.toInt & 255) // convert to unsigned "byte" by masking with 0b11111111
             .grouped(28 * 28)
             .map(chunk => Image(chunk.grouped(28).toArray))
             .toSeq
