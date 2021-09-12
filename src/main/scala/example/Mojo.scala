package example

import com.github.nscala_time
import com.github.nscala_time.time.Imports.DateTime

case class MojoValue[A](value: A) extends MeasurableValue
case class Mojo[A](mojoValue: MojoValue[A]) extends SensorValue {
  override val timeStamp: nscala_time.time.Imports.DateTime = DateTime.now()
}
