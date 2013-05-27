package LotkaVolterra

import processing.core._
import PConstants._
import PApplet._
import scala.collection.mutable.ListBuffer

class Main extends PApplet {
  
  override def setup {
    size(900, 900)
    colorMode(HSB, 100)
    frameRate(999)
    background(0)
    noLoop
  }

  override def draw {
    val param = (100.,75.)  //a,b. a is used in dy/dt, b is used in dx/dt
    val cycle = 5000
    val accuracy = 0.0001
    def plot(init:(Double,Double)){
      def rec(left:ListBuffer[(Double,Double)]){
        if(left.isEmpty) true
        else {
          stroke(init._1.asInstanceOf[Float],100,100);
          point((left.head._1).asInstanceOf[Float],height-(left.head._2).asInstanceOf[Float]);
          rec(left.tail)}
      }
      rec(RungeKutta.integrate({(x:Double,y:Double)=> {x*(param._2-y)}},{(x:Double,y:Double)=> {y*(-param._1+x)}},
                                                                                      init._1,init._2,accuracy,cycle))
    }
    for(i <- Range(5,param._1.asInstanceOf[Int]-1,5)) plot((i,i*param._2/param._1))
    stroke(param._1.asInstanceOf[Float],100,100)
    point(param._1.asInstanceOf[Float],height-param._2.asInstanceOf[Float])
    plot((1,1*param._2/param._1))
    saveFrame("b.png")
  }
}

object Main {
  def main(args:Array[String]) {
    PApplet.main(Array("LotkaVolterra.Main"))
  }
}

