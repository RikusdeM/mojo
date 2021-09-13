package example

case class MetersPerSecond(metersPerSecond:Float)
case class Speed(speed:MetersPerSecond) extends MeasurableValue
