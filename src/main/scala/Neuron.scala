import scala.util.Random
class Neuron(n: Int) {

  var inputs = new Array[Double](n)
  var weights = Array.fill(n + 1)(Random.nextDouble())
  val bias = 1.0
  var output = 0.0

  def fire = {
    val mac = inputs.zip(weights).map { case (a, b) => a * b } .reduce(_ + _) + bias * weights.last
    // if (mac > 0) 1 else 0
    // sigmod
    output = 1 / (1 + Math.exp(-mac))
    // do ReLU
  }

  override def toString = {
    "Input: " + inputs.mkString(" ") + "\n" +
      "Bias: " + bias + "\n" +
      "Weights: " + weights.mkString(" ") + "\n" +
      "Output: " + output
  }
}

object Neuron extends App {

  val p = new Neuron(2)
  println(p.toString)
  p.inputs = Array(0, 0)
  p.fire
  println(p.toString)
  p.inputs = Array(0, 1)
  p.fire
  println(p.toString)
  p.inputs = Array(1, 0)
  p.fire
  println(p.toString)
  p.inputs = Array(1, 1)
  p.fire
  println(p.toString)
}