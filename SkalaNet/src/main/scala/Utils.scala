package SkalaNet

object Utils:

    def zip[A, B, C](l1: IterableOnce[A], l2: IterableOnce[B], l3: IterableOnce[C]): Seq[(A, B, C)] =
        val (i1, i2, i3) = (l1.iterator, l2.iterator, l3.iterator)
        for i <- 0 until Seq(l1.size, l2.size, l3.size).max yield 
            (i1.next, i2.next, i3.next)
    
    case class progressBar[T](xs: IterableOnce[T], width: Int = 100) extends Iterator[T]:
        val it = xs.iterator
        var i = 1

        def hasNext: Boolean = it.hasNext

        def next(): T = 
            val completedRatio = i / xs.size.toFloat
            val numFill = (completedRatio * width).toInt
            print(f"\r[${"#" * numFill + " " * (width - numFill)}]  ${(completedRatio * 100)}%3.1f%%")
            if i == xs.size then println()
            i += 1
            it.next()
