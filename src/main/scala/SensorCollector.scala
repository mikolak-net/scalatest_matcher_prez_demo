

abstract class SensorCollector {

  def sensors: Seq[Sensor]

  def findByType(typeId: String): Option[Sensor] = {
    val  typeToFind = "blah" //debug
    sensors.find(_.name == typeToFind)
  }

}


abstract class Sensor(val name: String, val id: Long) {

  def value: Option[Double]

  def textValue = s"$name: ${value.getOrElse(0)}"

  def initialize: Unit

  def initialized: Boolean

}

class TemperatureSensor(id: Long) extends Sensor("Temp", id) {

  var initialized = false

  def value = if(initialized) Some(4.0) else None

  override def textValue = super.textValue+"K"

  def initialize = {
    initialized = true
  }

}

class BadTemperatureSensor(id: Long) extends Sensor("Temp", id) {

  var initialized = false

  def value = if(initialized) Some(4.0) else None

  override def textValue = super.textValue+"K"

  def initialize = {
    //TODO: will implement after lunch
  }

}

/**
 * Provides solar irradiance (insolation) data, with value capping based on latitude.
 */
class SolarIrradianceSensorSensor(override val id: Long, val latitude: Double) extends Sensor("Insol", id) {

  def value = Some(200.0) //take the global mean, bored now!

  override def textValue = super.textValue+"W/m2"

  val initialized = true

  def initialize = {
    //no op, always initialized
  }

}

class SingleSensorCollector(val sensor: Sensor) extends SensorCollector {

  val sensors = List(sensor)

}

class MultiSensorCollector(private var sensorList: List[Sensor]) extends SensorCollector {

  def sensors = sensorList

  def addSensor(sensor: Sensor) = sensorList :+= sensor

  def removeSensor(sensor: Sensor) = {} //sensorList = sensorList.filter(_ == sensor) TODO: (2014.03.01) was causing some problems, temporarily switched off

  def legacySensors: java.util.List[Sensor] = {
    import scala.collection.JavaConverters._
    sensors.asJava
  }

  def averageValue = sensorList.map(_.value.getOrElse(0.0)).sum / sensorList.size
}


object SensorCollectionDsl {

  def start = new Builder

  class Builder {

    var sensors = List.empty[Sensor]

    def withTemperatureSensor(initialValue: Double) = new TemperatureSensor(54) {
      initialized = true

      override def value = Some(initialValue)
    }

    //legacy code, of course we leave this commented out
//    def withTemperatureSensor(initialValue: String) = new TemperatureSensor(54) {
//      initialized = true
//
//      override def value = Some(initialValue.toDouble)
//    }

    def done() = new MultiSensorCollector(sensors)
  }

}


case class SensorDataPoint(timestamp: Long, value: Double)

class SensorDataDisplay(val list: List[SensorDataPoint]) {

  def sorted = list sortWith((one, other) => (one.value > other.value) || (one.timestamp < other.timestamp))


}