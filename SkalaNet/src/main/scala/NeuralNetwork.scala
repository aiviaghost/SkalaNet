package SkalaNet

//     W      *     X     +    B     =>  X_next
// (16 x 784) * (784 x 1) + (16 x 1) => (16 x 1)
// (16 x 16)  *  (16 x 1) + (16 x 1) => (16 x 1)
// (10 x 16)  *  (16 x 1) + (10 x 1) => (10 x 1)

case class NeuralNetwork (val layers: Int*):
    val layerDimensions = layers.zip(layers.tail)
    val weights = layerDimensions.map((n, m) => Matrix.fillRandom(n, m))
    val biases = layerDimensions.map((n, _) => Matrix.fillRandom(n, 1))

    // ReLU
    private def __/(x: Matrix): Matrix = x.map(math.max(_, 0))

    private def feedForward(inp: Matrix): Matrix = weights.zip(biases).foldLeft(inp){case (x, (w, b)) => __/(w * x + b)}
