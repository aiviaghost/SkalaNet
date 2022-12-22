package SkalaNet

import java.util.zip.{Inflater, Deflater}

object ZipWrapper:

    def compress(data: Array[Byte]): Array[Byte] =
        val deflater = Deflater()
        deflater.setInput(data)
        deflater.finish
        val outputBuffer = Array.ofDim[Byte](data.size)
        val compressedSize = deflater.deflate(outputBuffer)
        outputBuffer.take(compressedSize)

    def uncompress(data: Array[Byte]): Array[Byte] = ???
