package example

import com.github
import com.github.nscala_time.time.Imports.DateTime

object Calculations {

  def oldestTimestamp(
      timeStamps: List[DateTime]
  ): github.nscala_time.time.Imports.DateTime = {
    timeStamps match {
      case lst if lst.nonEmpty => lst.min
      case _                   => DateTime.now()
    }
  }

  def avgWeightPerSample(
      containerWeight: ContainerWeight
  )(numberOfSamples: NumberOfSamples): CalculationValue[Grams] = {
    if (numberOfSamples.number != 0) {
      CalculationValue[Grams](
        Grams(containerWeight.weight.grams / numberOfSamples.number),
        oldestTimestamp(
          containerWeight.timeStamp :: numberOfSamples.timeStamp :: Nil
        )
      )
    } else {
      CalculationValue[Grams](
        Grams(0),
        oldestTimestamp(
          containerWeight.timeStamp :: numberOfSamples.timeStamp :: Nil
        )
      )
    }
  }

  def calculate[A <: SensorValue, B <: MeasurableValue, C](
      sensorValue: A
  )(measurableValue: B)(
      f: A => B => CalculationValue[C]
  ): CalculationValue[C] = {
    val newCalculation = f(sensorValue)(measurableValue)
    CalculationValue(
      newCalculation.value,
      oldestTimestamp(sensorValue.timeStamp :: newCalculation.timeStamp :: Nil)
    )
  }

  def calculateMeasurable[A <: MeasurableValue, B <: MeasurableValue, C](
      measurableValue_1: A
  )(measurableValue_2: B)(
      f: A => B => CalculationValue[C]
  ): CalculationValue[C] = {
    val newCalculation = f(measurableValue_1)(measurableValue_2)
    CalculationValue(
      newCalculation.value,
      oldestTimestamp(newCalculation.timeStamp :: Nil)
    )
  }

}
