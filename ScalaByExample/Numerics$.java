/**
  * Created by hendrik.belitz on 02.05.2016.
  */
object Numerics {
  def sqrt(x:Double):Double = {
    def iterate(x:Double,guess:Double):Double = {
      val y = (guess+(x/guess))/2.0
      if ( Math.abs(y*y)-x < (0.01/Math.log10(x)) )
        y
      else {
        iterate(x, y)
      }
    }

    iterate(x,1.0)
  }

  def factorial(x:Int):Int = {
    def iter(acc:Int,x:Int):Int = {
      if (x <= 1) acc
      else {
        iter(acc*x,x-1)
      }
    }
    iter(1,x)
  }
}
