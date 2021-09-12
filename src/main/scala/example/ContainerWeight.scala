package example

import com.github.nscala_time.time
import com.github.nscala_time.time.Imports.DateTime
import scala.language.implicitConversions

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
