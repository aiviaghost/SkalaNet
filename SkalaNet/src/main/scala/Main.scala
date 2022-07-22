package SkalaNet

lazy val training_images = Image.readImages("../MNIST/training_set/train-images-idx3-ubyte")
lazy val test_images = Image.readImages("../MNIST/test_set/t10k-images-idx3-ubyte")

def testNetwork() = ???

def trainNetwork() = ???

def saveNetwork() = ???

def loadNetwork() = ???

def help() = ???

val menuOptions = Seq(
    ("Test network", testNetwork _),
    ("Train network", trainNetwork _),
    ("Save network", saveNetwork _),
    ("Load network", loadNetwork _),
    ("Help", help _)
)

def getUserChoice(): Option[Int] = 
    println(menuOptions.zipWithIndex.map((p, i) => s"${i + 1}. ${p._1}").mkString("\n"))
    io.StdIn.readLine("Your choice: ").toIntOption

@main def start(): Unit = 
    println("Welcome to SkalaNet!")
    while true do
        getUserChoice() match
            case Some(i) if 1 <= i && i <= menuOptions.length => menuOptions(i - 1)._2()
            case Some(_) => println(s"That is not a valid option! Please input an integer in the interval [1, ${menuOptions.length}]")
            case None => println(s"That is not an integer! Please input an integer in the interval [1, ${menuOptions.length}]")
