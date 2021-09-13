package example

protected trait MeasurableValue
case class Measurable[A](measurable:A) extends MeasurableValue