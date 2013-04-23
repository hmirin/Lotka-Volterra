package LotkaVolterra

import scala.collection.mutable.ListBuffer

object RungeKutta
{
  def integrate (f:(Double, Double) => Double,g:(Double,Double) => Double, x0:Double, y0: Double, accuracy:Double, cycle:Int) =
  {
  var ti:Double  = 0
  var x          = x0
  var y          = y0                                         // initialize y = f(t) to y0
  var k1x = 0.; var k2x = 0.; var k3x = 0.; var k4x = 0.
  var k1y = 0.; var k2y = 0.; var k3y = 0.; var k4y = 0.
  var trace = ListBuffer((x,y))
    for (i <- 1 to cycle) {
      k1x  = f (x, y)
      k1y  = g (x, y)
      k2x  = f (x + accuracy*k1x/2., y + accuracy*k1y/2.)
      k2y  = g (x + accuracy*k1x/2., y + accuracy*k1y/2.)
      k3x  = f (x + accuracy*k2x/2., y + accuracy*k2y/2.)
      k3y  = g (x + accuracy*k2x/2., y + accuracy*k2y/2.)
      k4x  = f (x + accuracy*k3x, y + accuracy*k3y)
      k4y  = g (x + accuracy*k3x, y + accuracy*k3y)
      x  += accuracy*(k1x + 2*k2x + 2*k3x + k4x) / 6.
      y  += accuracy*(k1y + 2*k2y + 2*k3y + k4y) / 6.
      trace +=((x,y))
    }
    trace
  }
}
