object MultilayerPerceptron extends App {

  val output = new Neuron(4)
  val hidden = Array.fill(4)(new Neuron(2))

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
    output.weights = output.weights.zip(hidden.map(_.output)).map { case (w, o) => w + err * o }
  }
}
