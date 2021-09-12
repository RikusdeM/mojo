package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Calculations._

class MojoSpec extends AnyFlatSpec with Matchers {

  val containerWeight: ContainerWeight = ContainerWeight(1000)
  Thread.sleep(1000)
  val numberOfSamples: NumberOfSamples = NumberOfSamples(6)
  Thread.sleep(1000)
  val temperature: Temperature = Temperature(22)
  val temperatureTimesWeight: Temperature => Grams => CalculationValue[Grams] =
    (temperature: Temperature) =>
      (grams: Grams) => {
        CalculationValue[Grams](
          Grams(temperature.temp.degrees * grams.grams),
          temperature.timeStamp
        )
      }

  "The AvgWeightPerSample calculation" should
    "contain the avg weight per sample using the last timestamp" in {

    println(s"$containerWeight : ${containerWeight.timeStamp.toString()}")
    println(s"$numberOfSamples : ${numberOfSamples.timeStamp.toString()}")

    val avg = avgWeightPerSample(containerWeight)(numberOfSamples)
    println(avg)
    avg === CalculationValue(Grams(0.16666667f), avg.timeStamp)
  }

  "Combining calculations of AvgWeight per sample with Temperature" should
    "return a CalculationValue of grams " in {

    val result = for {
      w <- avgWeightPerSample(containerWeight)(numberOfSamples)
      r2 <- calculate(temperature)(w)(temperatureTimesWeight)
    } yield {
      r2
    }
    result === CalculationValue(Grams(0.36666667f), containerWeight.timeStamp)
  }

  "Combining calculations of AvgWeight per sample with Temperature and Mojo" should
    "return a CalculationValue of DegreesCelsius " in {
    val mojo = Mojo(MojoValue(100))

    val mojoTimesTemperatureWeight = (mojo: Mojo[Int]) =>
      (weight: Grams) => {
        CalculationValue[Temperature](
          Temperature((mojo.mojoValue.value * weight.grams).toInt),
          temperature.timeStamp
        )
      }

    val result = for {
      w <- avgWeightPerSample(containerWeight)(numberOfSamples)
      r2 <- calculate(temperature)(w)(temperatureTimesWeight)
      r3 <- calculate(mojo)(r2)(mojoTimesTemperatureWeight)
    } yield {
      r3
    }

    result === CalculationValue(
      Temperature(DeciDegreeCelsius(36)),
      containerWeight.timeStamp
    )

  }
}
