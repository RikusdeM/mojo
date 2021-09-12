package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Calculations._
import org.joda.time.DateTime

class MojoSpec extends AnyFlatSpec with Matchers {

  val containerWeight: ContainerWeight = ContainerWeight(1000)
  Thread.sleep(1000)
  val numberOfSamples: NumberOfSamples = NumberOfSamples(6)

  "The AvgWeightPerSample calculation" should
    "contain the avg weight per sample using the last timestamp" in {

    println(s"$containerWeight : ${containerWeight.timeStamp.toString()}")
    println(s"$numberOfSamples : ${numberOfSamples.timeStamp.toString()}")

    val avg = avgWeightPerSample(containerWeight)(numberOfSamples)
    println(avg)
    avg === CalculationValue(Grams(0.16666667f),avg.timeStamp)
  }

  "Any calculation on " should "do something " in {
      val temperature: Temperature = Temperature(22)
      Thread.sleep(1000)

    val temperatureTimesWeight = (temperature: Temperature) =>
      (grams: Grams) => {
        CalculationValue[Grams](
          Grams(temperature.temp.degrees * grams.grams),
          temperature.timeStamp
        )
      }

    val result = for {
      w <- avgWeightPerSample(containerWeight)(numberOfSamples)
      r2 <- calculate(temperature)(w)(temperatureTimesWeight)
    } yield {
      r2
    }

    println(s"$temperature : ${temperature.timeStamp.toString()}")
    println(result)

  }


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
