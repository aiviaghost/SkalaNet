package SkalaNet

case class NeuralNetwork (val layers: Int*):
    val layerDimensions = layers.zip(layers.tail)
    val weights = layerDimensions.map((n, m) => Matrix.fillRandom(n, m))
    val biases = layerDimensions.map((n, _) => Matrix.fillRandom(n, 1))

    // ReLU
    private def __/(x: Matrix): Matrix = x.map(math.max(_, 0))

    private def feedforward(inp: Matrix): Matrix = weights.zip(biases).foldLeft(inp){case (x, (w, b)) => __/(w * x + b)}
