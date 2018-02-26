// https://twitter.com/tiarkrompf/status/963314799521222656/

import scala.util.continuations._

// sbt run
object AD extends App {
  type diff = cps[Unit]

  def grad(f: Num => Num @diff)(x: Double) = {
    val x1 = new Num(x, 0.0)
    reset { f(x1).d = 1.0 }
    x1.d
  }

  for (x <- 0 until 10) {
    assert(grad(x => x + x * x * x)(x) == 1 + 3 * x * x)
  }
}

class Num(val x: Double, var d: Double) {
  def +(that: Num) = shift { (k: Num => Unit) =>
    val y = new Num(x + that.x, 0.0)
    k(y)
    this.d += y.d
    that.d += y.d
  }

  def *(that: Num) = shift { (k: Num => Unit) =>
    val y = new Num(x * that.x, 0.0)
    k(y)
    this.d += that.x * y.d
    that.d += this.x * y.d
  }
}
