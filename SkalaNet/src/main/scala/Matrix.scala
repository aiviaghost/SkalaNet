package SkalaNet

case class Matrix private (private val M: Array[Array[Float]]):
    import scalanative.unsafe.*   
    
    private val (rows, cols) = (M.size, M(0).size)
    
    @extern
    private def mult(
        n: CInt, m: CInt, p: CInt, 
        A: Ptr[CFloat], B: Ptr[CFloat], 
        res: Ptr[CFloat]
    ): Unit = extern

    def *(other: Matrix): Matrix = 
        assert(cols == other.rows, "Dimensions are not valid for multiplication!")
        
        val (n, m, p) = (rows, cols, other.cols)
        
        val A = stackalloc[CFloat](n * m)
        for i <- 0 until n do
            for j <- 0 until m do
                !(A + i * m + j) = M(i)(j)
        
        val B = stackalloc[CFloat](m * p)
        for i <- 0 until m do
            for j <- 0 until p do
                !(B + i * p + j) = other.M(i)(j)
        
        val res = stackalloc[CFloat](n * p)
        mult(n, m, p, A, B, res)
        
        val newM = Array.fill(n)(Array.ofDim[Float](p))
        for i <- 0 until n do
            for j <- 0 until p do
                newM(i)(j) = !(res + i * p + j)
        Matrix(newM)

    override def toString(): String = M.map(_.mkString(", ")).mkString("\n")

object Matrix:

    def fillRandom(rows: Int, cols: Int): Matrix = 
        import util.Random.nextFloat
        Matrix(Array.fill(rows)(Array.fill(cols)(nextFloat())))
