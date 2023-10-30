class NeuralNetwork(val layers: List[Int]) {

  var neurons = List[List[Neuron]]()

  for (i <- 0 until layers.length) {
    neurons = neurons :+ List.fill(layers(i))(new Neuron(if (i == 0) 0 else layers(i - 1)))
  }

  def fire = {
    for (i <- 0 until layers.length) {
      for (j <- 0 until layers(i)) {
        neurons(i)(j).fire
      }
    }
  }

  def setInput(in: Array[Double]) = {
    for (i <- 0 until in.length) {
      neurons(0)(i).inputs(0) = in(i)
    }
  }

  def setExpect(exp: Double) = {
    // neurons.last.foreach(_.expect = exp)
  }

  def setAll(in: Array[Double], exp: Double) = {
    setInput(in)
    setExpect(exp)
  }

  def update = {
    for (i <- 0 until layers.length) {
      for (j <- 0 until layers(i)) {
        // neurons(i)(j).update
      }
    }
  }

  def learn() = {
    setAll(Array(0, 0), 0)
    fire
    update
    setAll(Array(0, 1), 1)
    fire
    update
    setAll(Array(1, 0), 1)
    fire
    update
    setAll(Array(1, 1), 1)
    fire
    update
  }

  override def toString = {
    var s = ""
    for (i <- 0 until layers.length) {
      s += "Layer " + i + "\n"
      for (j <- 0 until layers(i)) {
        s += neurons(i)(j).toString + "\n"
      }
    }
    s
  }
}
