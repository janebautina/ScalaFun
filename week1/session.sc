object session {

  def abs(x: Double) = if (x < 0) -x else x

  def sqrt(x: Double) = {
    def sqrtInter(guess: => Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtInter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.01
    def improve(guess: Double) =
      (guess + x / guess) / 2
    sqrtInter(1.0)
  }


  sqrt(2)
  sqrt(4)
  sqrt(1e-6)
}
