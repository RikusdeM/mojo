package example

import com.github
import com.github.nscala_time
import com.github.nscala_time.time
import com.github.nscala_time.time.Imports
import com.github.nscala_time.time.Imports.DateTime

import scala.language.implicitConversions

object MojoApp extends App {
  println("")
}

protected trait SensorValue {
  val timeStamp: DateTime
  //todo: Map & FlatMap
}
protected trait MeasurableValue

case class NumberOfSamples(number: Int) extends SensorValue {
  override val timeStamp: Imports.DateTime = DateTime.now()
}
object NumberOfSamples {
  def apply(number: Int): NumberOfSamples = {
    require(
      number <= 6 || number >= 0,
      "Number of samples should be in the range [0,6]"
    )
    new NumberOfSamples(number)
  }
}

case class DegreesCelsius(degrees: Float) extends MeasurableValue
case class DeciDegreeCelsius(deci: Int) extends MeasurableValue
object DeciDegreeCelsius {
  implicit def deciToDegrees(
      deciDegrees: DeciDegreeCelsius
  ): DegreesCelsius = DegreesCelsius(deciDegrees.deci.toFloat / 10)
}
case class Temperature(temp: DeciDegreeCelsius) extends SensorValue {
  override val timeStamp: Imports.DateTime = DateTime.now()
  private val degreesCelsius: Float = temp.degrees
}
object Temperature {
  def apply(deciDegrees: Int) = {
    new Temperature(DeciDegreeCelsius(deciDegrees))
  }
}

case class Grams(grams: Float) extends MeasurableValue
case class MilliGrams(milli: Int) extends MeasurableValue
object MilliGrams {
  implicit def milliToGrams(
      milliGrams: MilliGrams
  ): Grams = Grams(milliGrams.milli.toFloat / 1000)
}
case class ContainerWeight(weight: MilliGrams) extends SensorValue {
  override val timeStamp: time.Imports.DateTime = DateTime.now()
  private val grams: Float = weight.grams
}
object ContainerWeight {
  def apply(milliGrams: Int): ContainerWeight = {
    new ContainerWeight(MilliGrams(milliGrams))
  }
}

case class Mojo() extends SensorValue {
  override val timeStamp: nscala_time.time.Imports.DateTime = DateTime.now()
}

case class CalculationValue[A](value: A, timeStamp: DateTime) {
  import Calculations._
  def map[B](f: A => B): CalculationValue[B] =
    CalculationValue(f(value), this.timeStamp)
  def flatMap[B](f: A => CalculationValue[B]): CalculationValue[B] = {
    val newValue = f(value)
    CalculationValue(
      newValue.value,
      oldestTimestamp(this.timeStamp :: newValue.timeStamp :: Nil)
        .getOrElse(DateTime.now())
    )
  }
}

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
    //oldest timestamp
    CalculationValue(
      newCalculation.value,
      oldestTimestamp(sensorValue.timeStamp :: newCalculation.timeStamp :: Nil)
        .getOrElse(DateTime.now())
    )
  }
}
