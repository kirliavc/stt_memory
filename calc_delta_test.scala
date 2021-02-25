package stt

import breeze.linalg._
import breeze.numerics._

object CalcDeltaMain extends App {  // GEMM
  val a      = DenseMatrix((0.0, 0.0, 1.0))
  val t      = DenseMatrix((1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (1.0, 1.0, 1.0))
  val result = CalcDelta(a, t)
  println(result.length)
  for (i <- result)
    println(i)
}

object CalcDeltaMain2 extends App { // Conv, WS
  val a      = DenseMatrix(
      (0,1,0,0,0,0),
      (0,0,1,0,1,0),
      (0,0,0,1,0,1)
  ).mapValues(_.toDouble)
  val t      = DenseMatrix(
      (1,0,0,0,0,0),
      (0,1,0,0,0,0),
      (1,1,0,1,0,0),
      (0,0,0,0,0,1),
      (0,0,0,0,1,0),
      (0,0,1,0,0,0)
  ).mapValues(_.toDouble)
  val result = CalcDelta(a, t)
  println(result.length)
  for (i <- result)
    println(i)
}

object CalcDeltaMain3 extends App { // Conv, OS
  val a      = DenseMatrix(
      (0,1,0,0,0,0),
      (0,0,1,0,1,0),
      (0,0,0,1,0,1)
  ).mapValues(_.toDouble)
  val t      = DenseMatrix(
      (1,0,0,0,0,0),
      (0,0,0,1,0,0),
      (1,1,0,1,0,0),
      (0,0,0,0,0,1),
      (0,0,0,0,1,0),
      (0,0,1,0,0,0)
  ).mapValues(_.toDouble)
  val range = DenseVector(1,0,16,16,3,3)
  val stt_range = t.mapValues(_.toInt) * range
  println(stt_range)
  // 16, 16, 48, 3, 3, 16
  // (0, 0, 0, 0, 1, -1)
  // (0, 0, 1, -1, 0, 0)
  // 1, 0, 1, 0, 0, 0
  val result = CalcDelta(a, t)
  println(result.length)
  for (i <- result)
    println(i)
}