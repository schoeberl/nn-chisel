import scala.util.Random

/**
 * A simple perceptron, see https://en.wikipedia.org/wiki/Perceptron
 */
class Perceptron {
  var input = new Array[Double](2)
  var bias = 1.0
  var lr = 1.0
  var weights = Array.fill(3)(Random.nextDouble())
  var expect = 0.0

  def output = {
    val mac = input(0) * weights(0) + input(1) * weights(1) + bias * weights(2)
    // if (mac > 0) 1 else 0
    // sigmod
    1 / (1 + Math.exp(-mac))
  }
  override def toString(): String = {
    "Input: " + input.mkString(" ") + "\n" +
    "Bias: " + bias + "\n" +
    "Weights: " + weights.mkString(" ") + "\n" +
    "Output: " + output
  }

  def setWeights(w: Array[Double]) = {
    weights = w
  }
  def setInput(in: Array[Double]) = {
    input = in
  }

  def setExpect(exp: Double) = {
    expect = exp
  }

  def setAll(in: Array[Double], exp: Double) = {
    input = in
    expect = exp
  }
  def update = {
    val out = output
    val err = expect - out
    weights(0) += err * input(0)
    weights(1) += err * input(1)
    weights(2) += err * bias
  }

  def learn() = {
    setAll(Array(0, 0), 0)
    update
    setAll(Array(0, 1), 1)
    update
    setAll(Array(1, 0), 1)
    update
    setAll(Array(1, 1), 1)
    update
  }

}

object Perceptron extends App{

  val p = new Perceptron()
  println(p.toString())
  test
  println(p.toString())
  for (i <- 0 until 10) {
    p.learn()
    test()
    println(p.toString())
  }



  def test() = {
    p.setInput(Array(0, 0))
    println("0,0 -> " + (p.output + 0.5).toInt + "      "+ p.output)
    p.setInput(Array(0, 1))
    println("0,1 -> " + (p.output + 0.5).toInt + "      "+ p.output)
    p.setInput(Array(1, 0))
    println("1,0 -> " + (p.output + 0.5).toInt + "      "+ p.output)
    p.setInput(Array(1, 1))
    println("1,1 -> " + (p.output + 0.5).toInt + "      "+ p.output)
  }
}
