package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {
  def charMap(c: Char) = c match {
    case '(' => 1
    case ')' => -1
    case _ => 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    val n: Int = chars.length
    var sumValue: Int = 0

    var i: Int = 0
    while(i < n && sumValue >= 0){
      sumValue += charMap(chars(i))
      i += 1
    }

    return sumValue == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(from: Int, until: Int): (Int, Int) = {
      if(until <= from){
        (0, 0)
      }
      else{
        var sumValue: Int = 0
        var minValue = until - from
        
        var i: Int = from
        while(i < until){
          sumValue += charMap(chars(i))
          minValue = scala.math.min(minValue, sumValue)
          i += 1
        }

        (sumValue, minValue)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if(until - from <= threshold){
        traverse(from, until)
      }
      else{
        val mid = from + (until - from + 1) / 2
        val ((sumLeft, minLeft), (sumRight, minRight)) = parallel(reduce(from, mid), reduce(mid, until))
        (sumLeft + sumRight, scala.math.min(minLeft, sumLeft + minRight))
      }
    }

    val (sumValue, minValue) = reduce(0, chars.length)
    sumValue == 0 && minValue == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
