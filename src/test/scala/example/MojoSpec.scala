package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Calculations._

class MojoSpec extends AnyFlatSpec with Matchers {
  "The Mojo object" should "get the mojo" in {

    val containerWeight: ContainerWeight = ContainerWeight(1000)
    val numberOfSamples: NumberOfSamples = NumberOfSamples(6)
    val temperature: Temperature = Temperature(10)

    for {
      w <- avgWeightPerSample(containerWeight)(numberOfSamples)
      r2 <- calculate(temperature)(w)
    } yield {}

    C1(s1, s2).flatMap { r1 =>
      C2(s3, r1).flatMap { r2 => }
    }

    for {
      r1 <- C1(s1, s2) //avgWeightPerSample
      r2 <- C2(s3, r1)
    } yield {}

    C1(s1, s2).flatMap { r1 =>
      C2(s3, r1).flatMap { r2 => }
    }

  }
}
