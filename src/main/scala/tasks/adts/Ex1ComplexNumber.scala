package tasks.adts

/*  Exercise 1:
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    private case class ComplexImpl(re: Double, im: Double)
    opaque type Complex = ComplexImpl
    def complex(re: Double, im: Double): Complex = ComplexImpl(re, im)
    extension (complex: Complex)
      def re(): Double = complex.re
      def im(): Double = complex.im
      def sum(other: Complex): Complex = ComplexImpl(complex.re + other.re, complex.im + other.im)
      def subtract(other: Complex): Complex = ComplexImpl(complex.re - other.re, complex.im - other.im)
      def asString(): String = complex.re match
        case re if re == 0.0 => if complex.im == 0.0 then "0.0" else imToString(isPrecededByRe = false)
        case _ => s"${complex.re}${imToString(isPrecededByRe = true)}"

      private def imToString(isPrecededByRe: Boolean): String = 
        val sb = StringBuilder()
        complex.im match
          case im if im == 0.0 => return ""
          case im if im > 0.0 => if isPrecededByRe then sb.append(" + ")
          case _ => sb.append(if isPrecededByRe then " - " else "-")
        sb.append(Math.abs(complex.im))
        sb.append("i")
        sb.toString()
