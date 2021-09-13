package example

case class MetersSquared(metersSquare:Float)
case class Area(area:MetersSquared) extends MeasurableValue
