import org.scalatest.flatspec.AnyFlatSpec

class PerceptronTest extends AnyFlatSpec {
  "Perceptron" should "pass" in {
    val p = new Perceptron()
    for (i <- 0 until 10) {
      p.learn()
    }
    p.setInput(Array(0, 0))
    assert((p.output + 0.5).toInt == 0)
    p.setInput(Array(0, 1))
    assert((p.output + 0.5).toInt == 1)
    p.setInput(Array(1, 0))
    assert((p.output + 0.5).toInt == 1)
    p.setInput(Array(1, 1))
    assert((p.output + 0.5).toInt == 1)
  }
}
