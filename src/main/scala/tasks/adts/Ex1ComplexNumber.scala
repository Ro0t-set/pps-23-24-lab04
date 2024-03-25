package tasks.adts

package u04lab

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

    // Change assignment below: should probably define a case class and use it?
    type Complex = (Double,Double)
    def complex(re: Double, im: Double): Complex = (re:Double, im:Double)
    extension (complex: Complex)
      def re(): Double = complex match
        case (re:Double, im:Double) => re
      def im(): Double = complex match
        case (re:Double, im:Double) => im
      def sum(other: Complex): Complex =  complex match
        case (re:Double, im:Double) => (other.re() + complex.re(), other.im() + complex.im())

      def subtract(other: Complex): Complex = complex match
        case (re:Double, im:Double) => (complex.re() - other.re(), complex.im() - other.im())
      def asString(): String = complex match
        case (re:Double, im:Double) =>  val str = re.toString + " + " + im.toString + "i"
          if re.equals(0.0) || im.equals(0.0)
          then str.replace(" + 0.0i", "").replace("0.0 + ", "")
          else str.replace(" + -", " - ")

