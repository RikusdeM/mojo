package example

import com.github.nscala_time.time.Imports
import com.github.nscala_time.time.Imports.DateTime

case class NumberOfSamples(number: Int) extends SensorValue {
  override val timeStamp: Imports.DateTime = DateTime.now()
}

object NumberOfSamples {
  def apply(number: Int): NumberOfSamples = {
    require(
      number <= 6 && number >= 0,
      "Number of samples should be in the range [0,6]"
    )
    new NumberOfSamples(number)
  }
}
