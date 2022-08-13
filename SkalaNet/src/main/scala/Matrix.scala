package SkalaNet

import math.min
import util.Random.nextGaussian
import SkalaNet.Types.*

extension (M: Matrix)

    def rows: Int = M.size

    def cols: Int = M(0).size

    def +(other: Matrix): Matrix = 
        assert(rows == other.rows && cols == other.cols, "Matrix dimensions do not match!")

        val newM = Array.ofDim[Float](rows, cols)
        for i <- 0 until rows do 
            for j <- 0 until cols do
                newM(i)(j) = M(i)(j) + other(i)(j)
        
        newM

    def -(other: Matrix): Matrix = 
        assert(rows == other.rows && cols == other.cols, "Matrix dimensions do not match!")

        M + other * -1

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
                                res(i)(j) += M(i)(k) * other(k)(j)
        res

    def *(c: Float): Matrix = 
        M.map(_.map(z => c * z))
    
    def ⊙(other: Matrix): Matrix = 
        assert(rows == other.rows && cols == other.cols, "Matrix dimensions differ!")
        val res = Array.ofDim[Float](rows, cols)
        for i <- 0 until rows do
            for j <- 0 until cols do
                res(i)(j) = M(i)(j) * other(i)(j)
        res

object Matrix:

    def fillRandom(rows: Int, cols: Int): Matrix = 
        Array.fill(rows)(Array.fill(cols)(nextGaussian().toFloat))

    def zeros(rows: Int, cols: Int): Matrix = 
        Array.ofDim(rows, cols)
