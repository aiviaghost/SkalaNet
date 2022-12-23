package SkalaNet

import java.util.zip.{Inflater, Deflater}
import scala.collection.mutable.ArrayBuffer

object CompressionUtility:

    def compress(data: Array[Byte]): Array[Byte] =
        val deflater = Deflater()
        deflater.setInput(data)
        deflater.finish
        val outputBuffer = Array.ofDim[Byte](data.size)
        val compressedSize = deflater.deflate(outputBuffer)
        outputBuffer.take(compressedSize)

    def decompress(data: Array[Byte]): Array[Byte] =
        val inflater = Inflater()
        inflater.setInput(data)
        val outputBuffer = Array.ofDim[Byte](2 * data.size)
        var decompressedBytesCounter = Int.MaxValue
        val decompressedData = ArrayBuffer[Byte]()
        while decompressedBytesCounter > 0 do
            decompressedBytesCounter = inflater.inflate(outputBuffer)
            decompressedData.addAll(
                outputBuffer.take(decompressedBytesCounter)
            )
        decompressedData.toArray
