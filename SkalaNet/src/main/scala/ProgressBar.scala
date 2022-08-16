package SkalaNet

case class ProgressBar[T](
    xs: IterableOnce[T], 
    width: Int = 100, 
    name: String = "", 
    displayBar: Boolean = false, 
    displayTotalTime: Boolean = false,
    displayIterationMessage: Boolean = false,
    iterationMessage: (T, Int) => String = (a: T, b) => ""
) extends Iterator[T]:
    assert(displayBar || displayTotalTime || displayIterationMessage, "Progress bar needs to print something!")
    assert(!(displayBar && displayIterationMessage), "ProgressBar does not support displaying a progress bar and an iteration message simultaneously!")
    assert(!displayTotalTime || name != "", "Progress bar needs a name if it should print total time!")
    
    val it = xs.iterator
    var i = 1
    val t0 = System.nanoTime()
    
    def hasNext: Boolean = 
        if i == xs.size + 1 && displayTotalTime then
            val elapsedSeconds = (System.nanoTime() - t0) / 1e9
            println(f"${name} finished in ${elapsedSeconds}%.1f seconds")
        it.hasNext
    
    def next(): T = 
        val nextItem = it.next()
        if displayIterationMessage then
            println(iterationMessage(nextItem, i))
        if displayBar then
            val completedRatio = i / xs.size.toFloat
            val numFill = (completedRatio * width).toInt
            print(f"\r[${"#" * numFill + " " * (width - numFill)}]  ${(completedRatio * 100)}%.1f%%")
            if i == xs.size then println()
        i += 1
        nextItem
