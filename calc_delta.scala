package stt

import breeze.linalg._
import breeze.numerics._

import math.abs
import math.round
import scala.collection.mutable.ArrayBuffer
object add_reuse {
  def apply(vec: DenseVector[Double], group: ArrayBuffer[DenseVector[Double]]) = {
    println("in add_reuse")
    if(group.size==0){
      group += vec
    }
    else if(group.size==1){
      val itt = group(0)
      var same_line = true
      var p = 2.0/0.0
      var p_def = false
      for(i <- 0 until itt.length){
        if(itt(i)!=0 && vec(i)!=0){
          if(!p_def){
            p=itt(i)/vec(i)
            p_def = true
          }else if(itt(i)/vec(i)==p){
            same_line = false
          }
        }
      }
      if(same_line){
        group += vec
      }
    }
    else{   // 3 line in a space
      var same_line=true
      for(i <- 0 until group.size){
        for(j <- i+1 until group.size){
          val mat = DenseMatrix(vec, group(i), group(j))
          if(det(mat)==0)
            same_line=false
        }
      }
      if(same_line)
        group+=vec
    }
    println("reuse group:",group)
  }
}
object CalcDelta {
  def apply(matA: DenseMatrix[Double], matT: DenseMatrix[Double]) = {
    val matB = matA * inv(matT)
    // val matZ = DenseMatrix.eye[Double](matB.cols) - pinv(matB) * matB
    val d = ArrayBuffer.empty[DenseVector[Double]]

    def gen(n: Int, x: Array[Double]): List[DenseVector[Double]] = {
      if (n == matB.cols)
        new DenseVector[Double](x) :: Nil
      else
        gen(n + 1, x :+ 0.0) ++ gen(n + 1, x :+ 1.0) ++ gen(n + 1, x :+ -1.0)
    }
    println(matB)
    
    val testers = gen(0, Array.empty[Double])
    
    //println(testers)
    for (v <- testers) {
      //if (!d.exists((u) => norm(u dot v) > 1e-5)) {
        val y  = matB * v
        
        val ny = norm(y)
        val nv = norm(v)
        if (ny < 1e-5 && nv > 1e-5){
          println(y, v)
          d += v
        }
          
      //}
    }
    var df_v = ArrayBuffer.empty[DenseVector[Double]]
    var time_v = ArrayBuffer.empty[DenseVector[Double]]
    for(x <- d){
      
      if(x(0)==0 && x(1)==0){   // time reuse
        add_reuse(x, time_v)
      }
      var df_reuse = true
      for(i <- 3 until x.length){
        if(x(i)!=0)
          df_reuse = false
      }
      println(x, df_reuse)
      if(df_reuse){
        add_reuse(x, df_v)
      }
    }
    //if (matT.rows - rank(matB) != d.length)
    //  throw new Exception("Can not extract valid dataflows")
    d
  }
}
