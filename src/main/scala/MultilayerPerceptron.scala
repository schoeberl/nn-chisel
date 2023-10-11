object MultilayerPerceptron extends App {

  val output = new Neuron(10)
  val hidden = Array.fill(10)(new Neuron(2))

  def compute(in: Array[Double]) = {
    hidden.foreach { h =>
      h.inputs = in
      h.fire
    }
    output.inputs = hidden.map(_.output)
    output.fire
  }

  def learn(in: Array[Double], exp: Double) = {
    compute(in)
    val out = output.output
    val err = exp - out
    println(s"exp: $exp, out: $out, err: $err")
    // output.weights = output.weights.zip(hidden.map(_.output)).map { case (w, o) => w + err * o }
    for (i <- 0 until hidden.length) {
      output.weights(i) += err * hidden(i).output
    }
    output.weights(hidden.length) += err // * output.bias
  }

  def test() = {
    compute(Array(0, 0))
    println("0,0 -> " + (output.output + 0.5).toInt + "      "+ output.output)
    compute(Array(0, 1))
    println("0,1 -> " + (output.output + 0.5).toInt + "      "+ output.output)
    compute(Array(1, 0))
    println("1,0 -> " + (output.output + 0.5).toInt + "      "+ output.output)
    compute(Array(1, 1))
    println("1,1 -> " + (output.output + 0.5).toInt + "      "+ output.output)
  }

  // XOR does work (with 10 hidden neurons and 10000 iterations, but sometimes still fails)
  val input = Array[(Array[Double], Double)]((Array(0, 0), 0), (Array(0, 1), 1), (Array(1, 0), 1), (Array(1, 1), 0))

    for (i <- 0 until 10000) {
      input.foreach { case (in, exp) => learn(in, exp) }
      test()
      println(output.weights.mkString(" "))
    }
}
