package SkalaNet

import SkalaNet.Types.*

case class NeuralNetwork private (private val layerSizes: Seq[Int]):
    private val dimensions = layerSizes.tail.zip(layerSizes)
    private val weights = dimensions.map((n, m) => Matrix.fillRandom(n, m))
    private val biases = dimensions.map((n, _) => Matrix.fillRandom(n, 1))
    
    // ReLU ;)
    private def __/(m: Matrix): Matrix = m.map(_.map(z => math.max(z, 0)))

    private def feedforward(inp: Matrix): Matrix = 
        weights.zip(biases).foldLeft(inp){case (x, (w, b)) => __/(w * x + b)}

    // query the network using a matrix representing the image
    def apply(inp: Matrix): Int = 
        feedforward(inp).flatten.zipWithIndex.max._2

    // perform stochastic gradient descent
    def SGD(trainingData: Seq[(Matrix, Int)], epochs: Int, batchSize: Int): Unit = 
        import util.Random.shuffle
        val n = trainingData.size
        for epoch <- 1 to epochs do
            val shuffled = shuffle(trainingData)
            val miniBatches = (for i <- 0 until n by batchSize 
                                yield shuffled.slice(i, i + batchSize))
            for batch <- miniBatches do
                backprop(batch)
    
    private def backprop(batch: Seq[(Matrix, Int)]) = ???

object NeuralNetwork:

    def ofDim(layerSizes: Int*): NeuralNetwork = NeuralNetwork(layerSizes)
