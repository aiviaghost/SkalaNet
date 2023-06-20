package SkalaNet

import math.min
import util.Random.nextGaussian

opaque type Matrix = Array[Array[Float]]

extension (c: Int) def *(matrix: Matrix): Matrix = matrix * c
extension (c: Double) def *(matrix: Matrix): Matrix = matrix * c

extension (matrix: Matrix)
    def rows: Int = matrix.size

    def cols: Int = matrix(0).size

    def +(other: Matrix): Matrix =
        assert(
            rows == other.rows && cols == other.cols,
            "Matrix dimensions do not match!"
        )

        val newM = Array.ofDim[Float](rows, cols)
        for i <- 0 until rows do
            for j <- 0 until cols do newM(i)(j) = matrix(i)(j) + other(i)(j)

        newM

    def -(other: Matrix): Matrix =
        matrix + -1 * other

    def *(other: Matrix): Matrix =
        assert(
            cols == other.rows,
            s"Matrices with dimensions ${(rows, cols)} and ${(other.rows, other.cols)} can not be multiplied!"
        )
        val (n, m, p) = (rows, cols, other.cols)
        val T = 10
        val res = Array.ofDim[Float](n, p)
        for I <- 0 until n by T do
            for J <- 0 until p by T do
                for K <- 0 until m by T do
                    for i <- I until min(I + T, n) do
                        for j <- J until min(J + T, p) do
                            for k <- K until min(K + T, m) do
                                res(i)(j) += matrix(i)(k) * other(k)(j)
        res

    def *(c: Int): Matrix =
        matrix.map(_.map(z => c * z))

    def *(c: Double): Matrix =
        matrix.map(_.map(z => c.toFloat * z))

    def hadamard(other: Matrix): Matrix =
        assert(
            rows == other.rows && cols == other.cols,
            "Matrix dimensions differ!"
        )

        val res = Array.ofDim[Float](rows, cols)
        for i <- 0 until rows do
            for j <- 0 until cols do res(i)(j) = matrix(i)(j) * other(i)(j)
        res

    def T: Matrix = matrix.transpose

object Matrix:

    def fromArray(matrix: Array[Array[Float]]): Matrix =
        assert(
            matrix.forall(_.size == matrix(0).size),
            "Not all rows are of equal length!"
        )

        matrix

    def fillRandom(rows: Int, cols: Int): Matrix =
        Array.fill(rows)(Array.fill(cols)(nextGaussian().toFloat))

    def zeros(rows: Int, cols: Int): Matrix =
        Array.ofDim(rows, cols)

    def ones(rows: Int, cols: Int): Matrix =
        Array.fill(rows)(Array.fill(cols)(1f))

    def argmax(matrix: Matrix): Int =
        assert(matrix.rows == 1 || matrix.cols == 1, "Matrix is not a vector!")

        matrix.flatten.zipWithIndex.max._2

    def map(f: Float => Float, matrix: Matrix): Matrix =
        matrix.map(_.map(f))

    def makeTargetVector(rows: Int, targetIndex: Int): Matrix =
        val matrix = Array.ofDim[Float](rows, 1)
        matrix(targetIndex)(0) = 1f
        matrix
