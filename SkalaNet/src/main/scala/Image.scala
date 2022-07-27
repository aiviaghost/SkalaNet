package SkalaNet

case class Image private (val label: Int, private val pixels: Array[Array[Int]]):

    def toColumnVector(): Matrix = Matrix(pixels.flatten.map(pixel => Array(pixel / 255f)))

    override def toString(): String = 
        val byte2ascii = """ .'`^",:;Il!i><~+_-?][}{1)(|\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"""
        val digit = pixels.map("|" + _.map(pixel => byte2ascii(pixel * byte2ascii.size / 256)).mkString + "|").mkString("\n")
        val bar = "+" + "-".padTo(28, "-").mkString + "+" // "-".repeat(28) is not supported by Scala Native (https://github.com/scala-native/scala-native/pull/2711) :(
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
                            .map((pixels, label) => Image(pixels = pixels, label = label))
                            .toSeq
