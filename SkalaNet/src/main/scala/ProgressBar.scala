package SkalaNet

implicit def iterationMessageConvert[T](
    f: (T, Int) => String
): IterationMessage[T] = IterationMessage(f, true)

case class IterationMessage[T](
    f: (T, Int) => String,
    val displayIterationMessage: Boolean = false
):
    def apply(item: T, i: Int): String = f(item, i)

case class ProgressBar[T](
    xs: IterableOnce[T],
    width: Int = 80,
    name: String = "",
    displayBar: Boolean = false,
    displayTotalTime: Boolean = false,
    iterationMessage: IterationMessage[T] = IterationMessage((a: T, b) => ""),
    iterationName: String = "iteration"
) extends Iterator[T]:
    assert(
        displayBar || displayTotalTime || iterationMessage.displayIterationMessage,
        "Progress bar needs to print something!"
    )
    assert(
        !(displayBar && iterationMessage.displayIterationMessage),
        "ProgressBar does not support displaying a progress bar and an iteration message simultaneously!"
    )
    assert(
        !displayTotalTime || name != "",
        "Progress bar needs a name if it should print total time!"
    )

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

        if iterationMessage.displayIterationMessage then
            println(iterationMessage(nextItem, i))

        if displayBar then
            val completedRatio = i / xs.size.toFloat
            val numFill = (completedRatio * width).toInt
            val avg_time = (System.nanoTime() - t0) / (1e9 * i)
            val completedBar = "#" * numFill + " " * (width - numFill)
            print(
                f"\r[${completedBar}]  ${(completedRatio * 100)}%.1f%% | ${avg_time}%.4fs / ${iterationName}"
            )
            if i == xs.size then println()

        i += 1
        nextItem
