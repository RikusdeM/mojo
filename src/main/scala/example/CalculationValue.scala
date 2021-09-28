package example

import com.github.nscala_time.time.Imports.DateTime

case class CalculationValue[A](value: A, timeStamp: DateTime) {
  import Calculations._
  def map[B](f: A => B): CalculationValue[B] =
    CalculationValue(f(value), this.timeStamp)
  def flatMap[B](f: A => CalculationValue[B]): CalculationValue[B] = {
    val newValue = f(value)
    CalculationValue(
      newValue.value,
      oldestTimestamp(this.timeStamp :: newValue.timeStamp :: Nil)
    )
  }
}
