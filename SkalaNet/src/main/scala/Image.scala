package SkalaNet

import java.nio.file.{Files, Paths}

case class Image private (
    val label: Int,
    private val pixels: Array[Array[Int]]
):

    def toColumnVector(): Matrix =
        Matrix.fromArray(pixels.flatten.map(pixel => Array(pixel / 255f)))

    override def toString(): String =
        val byte2ascii =
            """ .'`^",:;Il!i><~+_-?][}{1)(|\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"""
        val digit = pixels
            .map(
                "|" + _.map(pixel =>
                    byte2ascii(pixel * byte2ascii.size / 256)
                ).mkString + "|"
            )
            .mkString("\n")
        val bar = "+" + "-" * 28 + "+"
        Seq(bar, digit, bar).mkString("\n")

object Image:

    private def readBytes(file: String): Array[Byte] =
        Files.readAllBytes(Paths.get(file))

    private def readLabels(labelFile: String): Array[Int] =
        readBytes(labelFile).drop(8).map(_.toInt)

    def readImages(imageFile: String, labelFile: String): IndexedSeq[Image] =
        val labels = readLabels(labelFile)
        readBytes(imageFile)
            .drop(16)
            .map(
                _.toInt & 255
            ) // convert to unsigned "byte" by masking with 0b11111111
            .grouped(28 * 28)
            .map(_.grouped(28).toArray)
            .zip(labels)
            .map((pixels, label) => Image(pixels = pixels, label = label))
            .toIndexedSeq
