package SkalaNet

import java.util.Base64
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import java.io.{
    FileInputStream,
    FileOutputStream,
    ObjectInputStream,
    ObjectOutputStream,
    ByteArrayInputStream,
    ByteArrayOutputStream
}
import scala.util.{Try, Success, Failure}

lazy val trainingImages = Image.readImages(
    imageFile = "../MNIST/training_set/train-images-idx3-ubyte",
    labelFile = "../MNIST/training_set/train-labels-idx1-ubyte"
)
lazy val testImages = Image.readImages(
    imageFile = "../MNIST/test_set/t10k-images-idx3-ubyte",
    labelFile = "../MNIST/test_set/t10k-labels-idx1-ubyte"
)

var nn = NeuralNetwork.ofDim(784, 16, 16, 10)

def scoreNetwork() =
    println("Scoring network accuracy ...")
    val score = testImages.count(img =>
        nn(img.toColumnVector()) == img.label
    ) / testImages.size.toDouble
    println(f"Network accuracy is ${score * 100}%3.2f%%.")

def tryNetwork() =
    val images = testImages.iterator
    var continue = true
    while images.hasNext && continue do
        val image = images.next()
        println(image)
        println(
            s"SkalaNet thinks this is the digit ${nn(image.toColumnVector())}."
        )
        println(s"The correct answer is ${image.label}.")
        println("Try another image? (y/n)")
        continue = io.StdIn.readLine("Your choice: ") == "y"

def trainNetwork() = nn.SGD(
    trainingData = trainingImages,
    epochs = 5,
    batchSize = 100
)

def saveNetwork() =
    val filename = io.StdIn.readLine("Filename: ")
    val outputStream = ByteArrayOutputStream()
    ObjectOutputStream(outputStream).writeObject(nn)

    if !Files.exists(Paths.get("../Saved-networks")) then
        Files.createDirectory(Paths.get("../Saved-networks"))

    Files.write(
        Paths.get(s"../Saved-networks/${filename}"),
        Base64.getEncoder.encode(
            CompressionUtility.compress(outputStream.toByteArray())
        )
    )

def loadNetwork() =
    val filename = io.StdIn.readLine("Filename: ")

    val data = Try {
        CompressionUtility.decompress(
            Base64.getDecoder.decode(
                Files.readAllBytes(
                    Paths.get(s"../Saved-networks/${filename}")
                )
            )
        )
    }

    data match
        case Success(data) =>
            val new_nn_t = Try {
                ObjectInputStream(ByteArrayInputStream(data))
                    .readObject()
                    .asInstanceOf[NeuralNetwork]
            }

            new_nn_t match
                case Success(new_nn) => nn = new_nn
                case Failure(_) =>
                    println(
                        "Failed to create NeuralNetwork instance from selected file!"
                    )

        case Failure(_) => println("Reading selected file failed!")

def quit() = System.exit(0)

val menuOptions = Seq(
    ("Try network", tryNetwork _),
    ("Score network", scoreNetwork _),
    ("Train network", trainNetwork _),
    ("Save network", saveNetwork _),
    ("Load network", loadNetwork _),
    ("Quit", quit _)
)

def getMenuChoice(): Option[Int] =
    println(
        menuOptions.zipWithIndex
            .map((p, i) => s"${i + 1}. ${p._1}")
            .mkString("\n")
    )
    io.StdIn.readLine("Your choice: ").toIntOption

@main def start(): Unit =
    println("Welcome to SkalaNet!")
    while true do
        getMenuChoice() match
            case Some(i) if 1 <= i && i <= menuOptions.length =>
                menuOptions(i - 1)._2()
            case Some(_) =>
                println(
                    s"That is not a valid option! Please input an integer in the interval [1, ${menuOptions.length}]"
                )
            case None =>
                println(
                    s"That is not an integer! Please input an integer in the interval [1, ${menuOptions.length}]"
                )
