package SkalaNet

case class NeuralNetwork (private val layers: Int*):
    private val dimensions = layers.zip(layers.tail)
    private val weights = dimensions.map((n, m) => Matrix.fillRandom(n, m))
    private val biases = dimensions.map((n, _) => Matrix.fillRandom(n, 1))

    // ReLU ;)
    private def __/(x: Matrix): Matrix = x.map(math.max(_, 0))

    private def feedforward(inp: Matrix): Matrix = weights.zip(biases).foldLeft(inp){case (x, (w, b)) => __/(w * x + b)}
