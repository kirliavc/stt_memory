package stt

import breeze.linalg._
import breeze.numerics._
//import scala.math._

object testT{
  def apply(t: DenseMatrix[Double]) = {
    val a      = DenseMatrix(
        (0,1,0,0,0,0),
        (0,0,1,0,1,0),
        (0,0,0,1,0,1)
    ).mapValues(_.toDouble)
  
    val range_ed = DenseVector(1,0,16,16,3,3)
    val range_st = DenseVector(1,0,0,0,0,0)
    val stt_range_st = t.mapValues(_.toInt) * range_st
    val stt_range_ed = t.mapValues(_.toInt) * range_ed
    // 16, 16, 48, 3, 3, 16
    // (0, 0, 0, 0, 1, -1)
    // (0, 0, 1, -1, 0, 0)
    // 1, 0, 1, 0, 0, 0
    val (time_v, df_v) = CalcDelta(a, t)
    val del_id = time_v.map(x=>{
      var last_id = -1
      for(i <- x.length - 1 to 0 by -1){
        if(x(i)!=0&&last_id == -1)
          last_id=i
      }
      last_id
    })
    val del_formula = for(i <- 0 until time_v.length)yield{
      (time_v(i) / (time_v(i)(del_id(i))/(-1))).mapValues(_.toInt)
    }
    val mem_size = for(i <- 0 until 6) yield{
      var max_i = max(stt_range_st(i),stt_range_ed(i))
      var min_i = min(stt_range_st(i),stt_range_ed(i))
      for(j <- 0 until time_v.length){
        val k = del_id(j)
        max_i = scala.math.max(max_i, max_i + del_formula(j)(i) * stt_range_st(k))
        max_i = scala.math.max(max_i, max_i + del_formula(j)(i) * stt_range_ed(k))
        min_i = scala.math.min(min_i, min_i + del_formula(j)(i) * stt_range_st(k))
        min_i = scala.math.min(min_i, min_i + del_formula(j)(i) * stt_range_ed(k))
      }

      if(del_id.exists(_==i)) 1 else max(1,max_i - min_i)
    }
    println("memory size:", mem_size.reduce(_*_))
  }
}
object CalcDeltaMain2 extends App { // Conv, WS
  for(x <- 0 until 512){
    val ii = for(i <- 0 until 9) yield{
      if ((x & (1<<i))==(1<<i)) 1 else 0
    }
    val t      = DenseMatrix(
        (ii(0),ii(1),0,ii(2),0,0),
        (ii(3),ii(4),0,ii(5),0,0),
        (ii(6),ii(7),0,ii(8),0,0),
        (0,0,0,0,0,1),
        (0,0,0,0,1,0),
        (0,0,1,0,0,0)
    ).mapValues(_.toDouble)
    // val t      = DenseMatrix(
    //     (ii(0),ii(1),0,ii(2),0,0),
    //     (ii(3),ii(4),0,ii(5),0,0),
    //     (ii(6),ii(7),0,ii(8),0,0),
    //     (0,0,0,0,0,1),
    //     (0,0,0,0,1,0),
    //     (0,0,1,0,0,0)
    // ).mapValues(_.toDouble)
    if(rank(t)==6)
      testT(t)
  }
  
}

// object CalcDeltaMain3 extends App { // Conv, OS
//   val a      = DenseMatrix(
//       (0,1,0,0,0,0),
//       (0,0,1,0,1,0),
//       (0,0,0,1,0,1)
//   ).mapValues(_.toDouble)
//   val t      = DenseMatrix(
//       (1,0,0,0,0,0),
//       (0,0,0,1,0,0),
//       (1,1,0,1,0,0),
//       (0,0,0,0,0,1),
//       (0,0,0,0,1,0),
//       (0,0,1,0,0,0)
//   ).mapValues(_.toDouble)
  
//   // val result = CalcDelta(a, t)
//   // println(result.length)
//   // for (i <- result)
//   //   println(i)
// }