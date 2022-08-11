package SkalaNet

import SkalaNet.Types.*
import collection.mutable.ArrayBuffer
import Utils.zip

extension (x: Float)
    def **(y: Int): Float = 
        math.pow(x.toDouble, y).toFloat

case class NeuralNetwork private (private val layerSizes: Seq[Int]):
    private val dimensions = layerSizes.tail.zip(layerSizes)
    private var weights = dimensions.map((n, m) => Matrix.fillRandom(n, m))
    private var biases = dimensions.map((n, _) => Matrix.fillRandom(n, 1))
    
    // ReLU ;)
    private def __/(m: Matrix): Matrix = m.map(_.map(z => math.max(z, 0)))

    private def reluPrime(m: Matrix): Matrix = m.map(_.map(z => if z > 0 then 1 else 0))

    private def feedforward(inp: Matrix): Matrix = 
        weights.zip(biases).foldLeft(inp){case (x, (w, b)) => __/(w * x + b)}

    private def costPrime(output: Matrix, expectedOutput: Matrix): Matrix = 
        (output - expectedOutput) * 2

    // query the network using a matrix representing the image
    def apply(inp: Matrix): Int = 
        feedforward(inp).flatten.zipWithIndex.max._2

    // perform stochastic gradient descent
    def SGD(trainingData: IndexedSeq[(Matrix, Int)], epochs: Int, batchSize: Int): Unit = 
        import util.Random.shuffle
        val n = trainingData.size
        for epoch <- 1 to epochs do
            val shuffled = shuffle(trainingData)
            val miniBatches = (for i <- 0 until n by batchSize 
                                yield shuffled.slice(i, i + batchSize))
            for batch <- miniBatches do
                processBatch(batch)
    
    private def processBatch(batch: Seq[(Matrix, Int)]): Unit = 
        var nablaW = (for w <- weights yield Matrix.zeros(w.rows, w.cols))
        var nablaB = (for b <- biases yield Matrix.zeros(b.rows, b.cols))
        for (m, ans) <- batch do
            val (deltaW, deltaB) = backprop(m, ans)
            nablaW = nablaW.zip(deltaW).map((nw, dw) => nw + dw)
            nablaB = nablaB.zip(deltaB).map((nb, db) => nb + db)
        val len = batch.size.toFloat
        weights = weights.zip(nablaW).map((w, nw) => w - nw * (1 / len))
        biases = biases.zip(nablaB).map((b, nb) => b - nb * (1 / len))

    private def backprop(inp: Matrix, expectedAns: Int): (Seq[Matrix], Seq[Matrix]) = 
        val deltaW = ArrayBuffer[Matrix]()
        val deltaB = ArrayBuffer[Matrix]()
        
        val zs = ArrayBuffer[Matrix]()
        val as = ArrayBuffer[Matrix](inp)
        weights.zip(biases).foldLeft(inp){
            case (x, (w, b)) => 
                val z = w * x + b
                zs.append(z)
                val a = __/(z)
                as.append(a)
                a
        }
        
        val expectedOutput = Array.ofDim[Float](10, 1)
        expectedOutput(expectedAns)(0) = 1f
        var delta = costPrime(as.last, expectedOutput) ⊙ reluPrime(zs.last)
        deltaW.append(delta * as.init.last.transpose)
        deltaB.append(delta)

        for (w_next, z, a_prev) <- zip(weights.tail, zs.init, as.init.init).reverse do
            delta = (w_next.transpose * delta) ⊙ reluPrime(z)
            deltaB.append(delta)
            deltaW.append(delta * a_prev.transpose)
        (deltaW.reverse.toSeq, deltaB.reverse.toSeq)

object NeuralNetwork:

    def ofDim(layerSizes: Int*): NeuralNetwork = NeuralNetwork(layerSizes)
