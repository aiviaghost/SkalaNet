package SkalaNet

import SkalaNet.Types.*
import scalanative.unsafe.*

@extern
def mult(
    n: CInt, m: CInt, p: CInt, 
    A: Ptr[CFloat], B: Ptr[CFloat], 
    res: Ptr[CFloat]
): Unit = extern

extension (M: Matrix)

    def rows: Int = M.size

    def cols: Int = M(0).size

    def +(other: Matrix): Matrix = 
        assert(rows == other.rows && cols == other.cols, "Matrix dimensions do not match!")

        val newM = Array.fill(rows)(Array.ofDim[Float](cols))
        for i <- 0 until rows do 
            for j <- 0 until cols do
                newM(i)(j) = M(i)(j) + other(i)(j)
        
        newM

    def -(other: Matrix): Matrix = 
        assert(rows == other.rows && cols == other.cols, "Matrix dimensions do not match!")

        val newM = Array.fill(rows)(Array.ofDim[Float](cols))
        for i <- 0 until rows do 
            for j <- 0 until cols do
                newM(i)(j) = M(i)(j) - other(i)(j)
        
        newM
        
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
                !(B + i * p + j) = other(i)(j)
        
        val res = stackalloc[CFloat](n * p)
        mult(n, m, p, A, B, res)
        
        val newM = Array.fill(n)(Array.ofDim[Float](p))
        for i <- 0 until n do 
            for j <- 0 until p do
                newM(i)(j) = !(res + i * p + j)
        
        newM
    
    def *(c: Float): Matrix = 
        M.map(_.map(z => c * z))

object Matrix:

    def fillRandom(rows: Int, cols: Int): Matrix = 
        import util.Random.nextFloat
        Array.fill(rows)(Array.fill(cols)(nextFloat()))

    def zeros(rows: Int, cols: Int): Matrix = 
        Array.fill(rows)(Array.ofDim(cols))
