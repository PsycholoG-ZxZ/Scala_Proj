import scala.reflect.ClassTag

class Polynomial[T](coeffs: Array[T]) {
  private val A = createCoeffs(coeffs)

  private def createCoeffs(A: Array[T]): Array[T] = {
    var s = A.length - 1
    while (s >= 0 && A(s) == 0)
      s -= 1
    A take (s+1)
  }

  def degree: Int = A.length - 1

  def coeff(i: Int): T = A(i)

  def printPol (): Unit = {
    var i = degree;

    while (i >= 0) {
      if (i == 0) {
        print("(" + coeff(i) + ")")
        i = i - 1
      } else {
        print("(" + coeff(i) + ")*x^" + i + " + ")
        i = i - 1

      }
    }
    println();
  }



    def der(a: Multt[T])(implicit m : ClassTag[T]) : Polynomial[T] = {
      val R = new Array[T](degree)
      var j = 0
      for (i <- 1 to degree) {
        //R(j) = i*coeff(i)
        R(j) = a.mult(i, coeff(i))
        j = j+1
      }
      new Polynomial(R)
    }

}

abstract class Multt[T]{
  def mult(a: Int, b: T):T
}

object Multt{
  object int extends Multt[Int]{
    override def mult(a: Int, b:Int):Int = a * b
  }
  object str extends Multt[String]{
    override def mult(a: Int, b:String):String = "" + a +"*("+ b + ")"
  }
  object flt extends Multt[Float]{
    override def mult(a: Int, b:Float):Float = a * b
  }
  object double extends Multt[Double]{
    override def mult(a: Int, b:Double):Double = a * b
  }


}

object Polynom {
  def main(args: Array[String]) {
    val a = Array(1,2,4)
    var poly1 = new Polynomial[Int](a)
    //println(poly1.dir)
    poly1.printPol()

    poly1 =  poly1.der(Multt.int)
    poly1.printPol()
    println("_______________________________")
    val b = Array("A", "B", "-C")
    var poly2 = new Polynomial[String](b)

    poly2.printPol()

    poly2.der(Multt.str).printPol()
    var poly3 = poly2.der(Multt.str)
    poly3.der(Multt.str).printPol()
    println("_______________________________")
    var poly4 = new Polynomial[Float](Array(1f,2.0f,3.5f))
    poly4.der(Multt.flt).printPol()



  }
}