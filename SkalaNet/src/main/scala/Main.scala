package SkalaNet

lazy val trainingImages = Image.readImages(
    imageFile = "../MNIST/training_set/train-images-idx3-ubyte",
    labelFile = "../MNIST/training_set/train-labels-idx1-ubyte"
)
lazy val testImages = Image.readImages(
    imageFile = "../MNIST/test_set/t10k-images-idx3-ubyte", 
    labelFile = "../MNIST/test_set/t10k-labels-idx1-ubyte"
)

val nn = NeuralNetwork.ofDim(784, 16, 16, 10)

def scoreNetwork() = 
    println("Scoring network accuracy. (This may take a while)")
    val score = testImages.count(img => nn(img.toColumnVector()) == img.label) / testImages.size.toDouble
    println(s"Network accuracy is ${score * 100}%.")

def tryNetwork() = 
    val images = testImages.iterator
    var continue = true
    while images.hasNext && continue do
        val image = images.next()
        println(image)
        println(s"SkalaNet thinks this is the digit ${nn(image.toColumnVector())}.")
        println(s"The correct answer is ${image.label}.")
        println("Try another image? (y/n)")
        continue = io.StdIn.readLine("Your choice: ") == "y"


def trainNetwork() = nn.SGD(
        trainingData = trainingImages.map(_.toColumnVector()), 
        epochs = 1,
        batchSize = 100
    )

def saveNetwork() = ???

def loadNetwork() = ???

def help() = ???

def quit() = System.exit(0)

val menuOptions = Seq(
    ("Try network", tryNetwork _),
    ("Score network", scoreNetwork _),
    ("Train network", trainNetwork _),
    ("Save network", saveNetwork _),
    ("Load network", loadNetwork _),
    ("Help", help _), 
    ("Quit", quit _)
)

def getMenuChoice(): Option[Int] = 
    println(menuOptions.zipWithIndex.map((p, i) => s"${i + 1}. ${p._1}").mkString("\n"))
    io.StdIn.readLine("Your choice: ").toIntOption

@main def start(): Unit = 
    println("Welcome to SkalaNet!")
    while true do
        getMenuChoice() match
            case Some(i) if 1 <= i && i <= menuOptions.length => menuOptions(i - 1)._2()
            case Some(_) => println(s"That is not a valid option! Please input an integer in the interval [1, ${menuOptions.length}]")
            case None => println(s"That is not an integer! Please input an integer in the interval [1, ${menuOptions.length}]")
