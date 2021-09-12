package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Calculations._
import org.joda.time.DateTime

class MojoSpec extends AnyFlatSpec with Matchers {
  "The Mojo object" should "get the mojo" in {

    val containerWeight: ContainerWeight = ContainerWeight(1000)
    Thread.sleep(1000)
    val numberOfSamples: NumberOfSamples = NumberOfSamples(6)
    Thread.sleep(1000)
    val temperature: Temperature = Temperature(10)

    val temperatureTimesWeight = (temperature:Temperature) => (grams:Grams) => {
      CalculationValue(temperature.temp.degrees * grams.grams, temperature.timeStamp)
    }

    val result = for {
      w <- avgWeightPerSample(containerWeight)(numberOfSamples)
      r2 <- calculate(temperature)(w)(temperatureTimesWeight)
    } yield {
      r2
    }

    println(s"$containerWeight : ${containerWeight.timeStamp.toString()}")
    println(s"$numberOfSamples : ${numberOfSamples.timeStamp.toString()}")
    println(s"$temperature : ${temperature.timeStamp.toString()}")

    val avg = avgWeightPerSample(containerWeight)(numberOfSamples)
    println(avg)


    println(result)


//    C1(s1, s2).flatMap { r1 =>
//      C2(s3, r1).flatMap { r2 => }
//    }
//
//    for {
//      r1 <- C1(s1, s2) //avgWeightPerSample
//      r2 <- C2(s3, r1)
//    } yield {}
//
//    C1(s1, s2).flatMap { r1 =>
//      C2(s3, r1).flatMap { r2 => }
//    }

  }
}
