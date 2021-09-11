package example

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

case class DegreesCelsius(degrees: Float)
case class DeciDegreeCelsius(deci: Int)
object DeciDegreeCelsius {
  implicit def deciToDegrees(
      deciDegrees: DeciDegreeCelsius
  ): DegreesCelsius = DegreesCelsius(deciDegrees.deci.toFloat / 10)
}
case class Temperature(temp: DeciDegreeCelsius) extends SensorValue {
  override val timeStamp: Imports.DateTime = DateTime.now()
  private val degreesCelsius: Float = temp.degrees
}

case class Grams(grams: Float)
case class MilliGrams(milli: Int)
object MilliGrams {
  implicit def milliToGrams(
      milliGrams: MilliGrams
  ): Grams = Grams(milliGrams.milli.toFloat / 1000)
}
case class ContainerWeight(weight: MilliGrams) extends SensorValue {
  override val timeStamp: time.Imports.DateTime = DateTime.now()
  private val grams: Float = weight.grams
}

case class Mojo() extends SensorValue {
  override val timeStamp: nscala_time.time.Imports.DateTime = DateTime.now()
}
