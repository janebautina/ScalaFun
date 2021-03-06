object rational {
  val x = new Rational(1, 2)
  x.numer
  x.denom
  val y = new Rational(2, 3)
  -y
  y + y
  x + y
  val z = new Rational(3, 2)
  x + y + z
  x - y - z
  x.max(y)
  new Rational(2)

  /**
    * Rational Numbers Representation
    *
    * @param i  - numerator value
    * @param i1  - denominator value
    */
  class Rational(i: Int, i1: Int) {
    require(i1 != 0, "denominator must not be equal zero")

    def this(x:Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    val numer = i

    val denom = i1

    def +(that: Rational): Rational =
      new Rational(that.denom * numer + denom * that.numer, that.denom * denom)

    override def toString = {
      def g = gcd(numer, denom)
      numer/g + "/" + denom/g
    }

    def unary_- = new Rational(-numer, denom)

    def -(that: Rational) = this + (-that)

    def <(that: Rational) = numer * that.denom < that.numer * denom

    def max(that: Rational) = if (this < that) that else this
  }
}
