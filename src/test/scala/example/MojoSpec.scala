package example

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.mockito.scalatest.IdiomaticMockito
import Calculations._
import com.github.nscala_time.time.Imports.DateTime

class MojoSpec extends AnyFlatSpec with Matchers with IdiomaticMockito {

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
    "contain the avg weight per sample using the oldest timestamp" in {

    println(s"$containerWeight : ${containerWeight.timeStamp.toString()}")
    println(s"$numberOfSamples : ${numberOfSamples.timeStamp.toString()}")

    val avg = avgWeightPerSample(containerWeight)(numberOfSamples)
    println(avg)
    avg === CalculationValue(Grams(0.16666667f), containerWeight.timeStamp)
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

  "Calculating the evaporation of samples from the surface of the container" should
    "return the amount [Kg/s]" in {

    /**
      * https://www.engineeringtoolbox.com/evaporation-water-surface-d_690.html
      */

    val temperature: Temperature = Temperature(250)

    val evaporationCoef
        : Temperature => Speed => CalculationValue[Measurable[Float]] =
      (temp: Temperature) => {
        (windSpeed: Speed) => {
          CalculationValue(
            Measurable(
              temp.temp.degrees + (19 * windSpeed.speed.metersPerSecond)
            ),
            DateTime.now()
          )
        }
      }

    val evaporationCoefTimesArea
        : Measurable[Float] => Area => CalculationValue[Measurable[Float]] =
      (evaporationCoef: Measurable[Float]) => {
        (area: Area) => {
          CalculationValue(
            Measurable(evaporationCoef.measurable * area.area.metersSquare),
            DateTime.now()
          )
        }
      }

    val saturationMinusHumidityRatio
        : SaturationHumidityRatio => Measurable[Float] => CalculationValue[
          Measurable[Float]
        ] =
      (saturation: SaturationHumidityRatio) => {
        (humidityRatio: Measurable[Float]) => {
          CalculationValue(
            Measurable(
              saturation.satHumRatio.kgPerKg - humidityRatio.measurable
            ),
            DateTime.now()
          )
        }
      }

    val evaporationTempo =
      (evaporationCoefTimesArea: Measurable[Float]) => {
        (saturationMinusHumidityRatio: Measurable[Float]) => {
          CalculationValue(
            (evaporationCoefTimesArea.measurable * saturationMinusHumidityRatio.measurable) / 3600,
            DateTime.now()
          )
        }
      }

    val area = Area(MetersSquared(50.0f * 20.0f)) // metersSquared
    val speed = Speed(MetersPerSecond(0.5f))
    val humidityRatio = Measurable[Float](0.0098f)

    val saturation = SaturationHumidityRatio(KgPerKg(0.014659f))
    val result = for {
      c1 <- calculate(temperature)(speed)(evaporationCoef)
      c2 <- calculateMeasurable(c1)(area)(evaporationCoefTimesArea)
      c3 <- calculate(saturation)(humidityRatio)(saturationMinusHumidityRatio)
      c4 <- calculateMeasurable(c2)(c3)(evaporationTempo)
    } yield {
      c4
    }
    result === CalculationValue(0.046565413, temperature.timeStamp)
  }
}
