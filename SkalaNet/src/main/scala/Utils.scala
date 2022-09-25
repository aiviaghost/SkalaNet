package SkalaNet

object Utils:

    def zip[A, B, C](
        l1: IterableOnce[A], 
        l2: IterableOnce[B], 
        l3: IterableOnce[C]
    ): Seq[(A, B, C)] =
        val (i1, i2, i3) = (l1.iterator, l2.iterator, l3.iterator)
        for i <- 0 until Seq(l1.size, l2.size, l3.size).max yield 
            (i1.next, i2.next, i3.next)
