package example

import com.github.nscala_time.time.Imports
import com.github.nscala_time.time.Imports.DateTime
import scala.language.implicitConversions

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
