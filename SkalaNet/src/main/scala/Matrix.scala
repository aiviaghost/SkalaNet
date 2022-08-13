package SkalaNet

import math.min
import util.Random.nextGaussian

extension [T](x: T)(using n: Numeric[T])

    def *(m: Matrix): Matrix = m * x

case class Matrix private (private val M: Array[Array[Float]]):

    def rows: Int = M.size

    def cols: Int = M(0).size

    def +(other: Matrix): Matrix = 
        assert(rows == other.rows && cols == other.cols, "Matrix dimensions do not match!")
        
        val newM = Array.ofDim[Float](rows, cols)
        for i <- 0 until rows do 
            for j <- 0 until cols do
                newM(i)(j) = M(i)(j) + other.M(i)(j)
        
        Matrix(newM)

    def -(other: Matrix): Matrix = 
        this + other * -1

    def *(other: Matrix): Matrix = 
        assert(cols == other.rows)
        val (n, m, p) = (rows, cols, other.cols)
        val T = 10
        val res = Array.ofDim[Float](n, p)
        for I <- 0 until n by T do
            for J <- 0 until p by T do
                for K <- 0 until m by T do
                    for i <- I until min(I + T, n) do
                        for j <- J until min(J + T, p) do
                            for k <- K until min(K + T, m) do
                                res(i)(j) += M(i)(k) * other.M(k)(j)
        Matrix(res)

    def *[T](c: T)(using n: Numeric[T]): Matrix = 
        import n.mkNumericOps
        Matrix(M.map(_.map(z => c.toFloat * z)))
    
    def ⊙(other: Matrix): Matrix = 
        assert(rows == other.rows && cols == other.cols, "Matrix dimensions differ!")

        val res = Array.ofDim[Float](rows, cols)
        for i <- 0 until rows do
            for j <- 0 until cols do
                res(i)(j) = M(i)(j) * other.M(i)(j)
        Matrix(res)
    
    def transpose: Matrix = Matrix(M.transpose)

object Matrix:

    def fromArray(m: Array[Array[Float]]): Matrix = Matrix(m)

    def fillRandom(rows: Int, cols: Int): Matrix = 
        Matrix(Array.fill(rows)(Array.fill(cols)(nextGaussian().toFloat)))

    def zeros(rows: Int, cols: Int): Matrix = 
        Matrix(Array.ofDim(rows, cols))
    
    def ones(rows: Int, cols: Int): Matrix = 
        Matrix(Array.fill(rows)(Array.fill(cols)(1f)))
    
    def argmax(m: Matrix): Int = 
        assert(m.cols == 1, "Matrix is not a column vector!")

        m.M.flatten.zipWithIndex.max._2
    
    def map(f: Float => Float, m: Matrix): Matrix = 
        Matrix(m.M.map(_.map(z => f(z))))
