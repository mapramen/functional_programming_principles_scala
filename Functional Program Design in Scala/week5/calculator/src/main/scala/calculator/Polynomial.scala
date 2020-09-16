package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    def A: Double = a()
    def B: Double = b()
    def C: Double = c()
    
    Var {
      B * B - 4 * A * C
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def A: Double = a()
    def B: Double = b()
    def Delta: Double = delta()
    
    Var {
      if(Delta < 0) {
        Set()
      }
      else{
        def D: Double = scala.math.sqrt(Delta)
        Set((-B - D) / (2 * A), (-B + D) / (2 * A))
      }
    }
  }
}
