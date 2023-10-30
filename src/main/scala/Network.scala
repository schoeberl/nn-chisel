class Network(val size: List[Int]) {

  val numLayers = size.length
  val layers = Array.fill(numLayers)(new Array[Neuron](size.length))
}

object Network extends App {

  val n = new Network(List(2, 3, 1))
  println(n)
}
