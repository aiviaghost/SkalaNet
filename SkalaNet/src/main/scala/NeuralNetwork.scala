package SkalaNet

import collection.mutable.ArrayBuffer
import Utils.*
import util.Random.shuffle

@SerialVersionUID(42L)
case class NeuralNetwork private (private val layerSizes: Seq[Int])
    extends Serializable:
    private val dimensions = layerSizes.tail.zip(layerSizes)
    private var weights = dimensions.map((n, m) => Matrix.fillRandom(n, m))
    private var biases = dimensions.map((n, _) => Matrix.fillRandom(n, 1))

    private def sigmoid(m: Matrix): Matrix =
        Matrix.map(z => 1 / (1 + math.exp(-z).toFloat), m)

    private def sigmoidPrime(m: Matrix): Matrix =
        sigmoid(m) ⊙ (Matrix.ones(m.rows, m.cols) - sigmoid(m))

    private def feedforward(inp: Matrix): Matrix =
        weights.zip(biases).foldLeft(inp) { case (x, (w, b)) =>
            sigmoid(w * x + b)
        }

    private def costPrime(output: Matrix, expectedOutput: Matrix): Matrix =
        2 * (output - expectedOutput)

    // query the network using a matrix representing the image
    def apply(inp: Matrix): Int =
        Matrix.argmax(feedforward(inp))

    // perform stochastic gradient descent
    def SGD(
        trainingData: IndexedSeq[Image],
        epochs: Int,
        batchSize: Int,
        eta: Float = 1
    ): Unit =
        ProgressBar(
            1 to epochs,
            displayTotalTime = true,
            name = "Training",
            iterationMessage = (_: Int, i: Int) => s"Epoch ${i} / ${epochs}:"
        ).foreach { epoch =>
            val shuffled = shuffle(trainingData)
            val miniBatches =
                (0 until trainingData.size by batchSize).map(i =>
                    shuffled.slice(i, i + batchSize)
                )
            ProgressBar(
                miniBatches,
                displayBar = true,
                iterationName = "batch"
            ).foreach { batch =>
                val (nablaW, nablaB) = processBatch(
                    batch.map(img => (img.toColumnVector(), img.label))
                )
                weights = weights
                    .zip(nablaW)
                    .map((w, nw) => w - eta * (1.0 / batchSize) * nw)
                biases = biases
                    .zip(nablaB)
                    .map((b, nb) => b - eta * (1.0 / batchSize) * nb)
            }
        }

    private def processBatch(
        batch: IndexedSeq[(Matrix, Int)]
    ): (Seq[Matrix], Seq[Matrix]) =
        var nablaW = (for w <- weights yield Matrix.zeros(w.rows, w.cols))
        var nablaB = (for b <- biases yield Matrix.zeros(b.rows, b.cols))

        for (m, ans) <- batch do
            val (deltaW, deltaB) = backprop(m, ans)
            nablaW = nablaW.zip(deltaW).map((nw, dw) => nw + dw)
            nablaB = nablaB.zip(deltaB).map((nb, db) => nb + db)

        (nablaW, nablaB)

    private def backprop(
        inp: Matrix,
        expectedAns: Int
    ): (Seq[Matrix], Seq[Matrix]) =
        val deltaW = ArrayBuffer[Matrix]()
        val deltaB = ArrayBuffer[Matrix]()

        val zs = ArrayBuffer[Matrix]()
        val as = ArrayBuffer[Matrix](inp)
        weights.zip(biases).foldLeft(inp) { case (x, (w, b)) =>
            val z = w * x + b
            zs.append(z)
            val a = sigmoid(z)
            as.append(a)
            a
        }

        val targetVector = Matrix.makeTargetVector(layerSizes.last, expectedAns)
        var delta = costPrime(as.last, targetVector) ⊙ sigmoidPrime(zs.last)
        deltaW.append(delta * as.init.last.transpose)
        deltaB.append(delta)

        for (wNext, z, aPrev) <- zip(
                weights.tail,
                zs.init,
                as.init.init
            ).reverse
        do
            delta = (wNext.transpose * delta) ⊙ sigmoidPrime(z)
            deltaB.append(delta)
            deltaW.append(delta * aPrev.transpose)

        (deltaW.reverse.toSeq, deltaB.reverse.toSeq)

object NeuralNetwork:

    def ofDim(layerSizes: Int*): NeuralNetwork = NeuralNetwork(layerSizes)
