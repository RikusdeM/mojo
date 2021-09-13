package example
import com.github.nscala_time.time.Imports
import com.github.nscala_time.time.Imports.DateTime

case class KgPerKg(kgPerKg:Float) extends MeasurableValue
case class SaturationHumidityRatio(satHumRatio:KgPerKg) extends SensorValue {
  override val timeStamp: Imports.DateTime = DateTime.now()
}
