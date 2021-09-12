package example

import com.github
import com.github.nscala_time.time.Imports.DateTime

object Calculations {

  def oldestTimestamp(
      timeStamps: List[DateTime]
  ): Option[github.nscala_time.time.Imports.DateTime] = {
    timeStamps.sorted.headOption
  }

  def avgWeightPerSample(
      containerWeight: ContainerWeight
  )(numberOfSamples: NumberOfSamples): CalculationValue[Grams] =
    CalculationValue[Grams](
      Grams(containerWeight.weight.grams / numberOfSamples.number),
      oldestTimestamp(
        containerWeight.timeStamp :: numberOfSamples.timeStamp :: Nil
      ).getOrElse(DateTime.now())
    )

  def calculate[A <: SensorValue, B <: MeasurableValue, C](
      sensorValue: A
  )(measurableValue: B)(
      f: A => B => CalculationValue[C]
  ): CalculationValue[C] = {
    val newCalculation = f(sensorValue)(measurableValue)
    CalculationValue(
      newCalculation.value,
      oldestTimestamp(sensorValue.timeStamp :: newCalculation.timeStamp :: Nil)
        .getOrElse(DateTime.now())
    )
  }
}
