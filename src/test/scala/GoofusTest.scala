import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class GoofusTest extends FlatSpec with MustMatchers with PropertyChecks {


  case class DummySensor(override val name: String, override val id: Long, valueFunc: () => Double) extends Sensor(name, id) {

    var initialized = false

    def value = Some(valueFunc())

    def initialize = {initialized = true}

  }

  "A Temperature sensor" must "store its ID" in {
    val id = 42l
    val tested = new TemperatureSensor(id)

    tested.id mustBe(id)
  }

  it must "the same as another sensor with the same id" in {
    val id = 7l
    val tested = new TemperatureSensor(id)
    val reference = DummySensor("blah", id, () => 0)

    tested must be(reference)
  }

  "A SingleSensorCollector" must "be the same reference as" in {
    val sensor = DummySensor("blah", 7, () => 10)
    val tested = new SingleSensorCollector(sensor)

    tested.sensors.head eq sensor must be(true)
  }


  it must "not throw an exception when adding a sensor" in {
    new SingleSensorCollector(new DummySensor("blah", 7, () => 10))
    ???
  }

  it must "throw an IllegalArgumentException when attempting to add a null sensor" in {
    new SingleSensorCollector(null)
    ???
  }

  "A TemperatureSensor's value" must "be within a range of +-3 when freshly initialized" in {
    val tested = new TemperatureSensor(0l)
    tested.initialize
    tested.value.get must be <= 3.0
    tested.value.get must be >= -3.0
  }

  "A MultiSensorCollection" must "produce a complete collection" in {
    val testData = List(DummySensor("blah", 10, () => 0), DummySensor("blah", 7, () => 0), DummySensor("blah", 8, () => 0))

    val tested = new MultiSensorCollector(List.empty)
    testData.foreach(tested.addSensor)


    for (testDatum <- testData) {
      tested.sensors.contains(testDatum) must be(true)
    }

    tested.sensors.size must be(testData.size)
  }

  it must "add sensors in order" in {
    val testData = List(DummySensor("blah", 10, () => 0), DummySensor("blah", 7, () => 0), DummySensor("blah", 8, () => 0))

    val tested = new MultiSensorCollector(List.empty)
    testData.foreach(tested.addSensor)

    tested.sensors must be(testData)
  }

  it must "return an empty list when provided with an empty seed" in {
    new MultiSensorCollector(List.empty).sensors.isEmpty must be(true)
  }

  it must "remove an element provided" in {
    val testData = List(DummySensor("blah", 10, () => 0), DummySensor("blah", 7, () => 0), DummySensor("blah", 8, () => 0))
    val deletedItem = testData(1)

    val tested = new MultiSensorCollector(List.empty)
    testData.foreach(tested.addSensor)

    tested.removeSensor(deletedItem)

    tested.sensors.contains(deletedItem) must be(false)
  }

  it must "return a Java list with sth" in {
    val testData = List(DummySensor("blah", 10, () => 0), DummySensor("blah", 7, () => 0), DummySensor("blah", 8, () => 0))

    val tested = new MultiSensorCollector(List.empty)
    testData.foreach(tested.addSensor)

    tested.legacySensors must be(testData) //surely, this will be equal!
  }

  it must "return the same type of a sensor as searched" in {
    val searchedName = "something"
    val tested = new MultiSensorCollector(List(DummySensor(searchedName, 10, () => 0)))

    tested.findByName(searchedName).get.name must be(searchedName)
  }


  it must "return the first matching sensors as searched" in {
    val searchedName = "something"
    val firstId = 10l
    val tested = new MultiSensorCollector(List(DummySensor(searchedName, firstId, () => 0),DummySensor(searchedName, 11, () => 0)))

    tested.findByName(searchedName).get.name must be(searchedName)
    tested.findByName(searchedName).get.id must be(firstId)
  }


  "A TemperatureSensor's text output" must "have be prefixed with the name of the sensor" in {
    new TemperatureSensor(0).textValue.startsWith("Temp") must be(true)
  }

  it must "output a sensible start value in text" in {
    val regexWord = raw"Temp: (\d+)K"
    new TemperatureSensor(30).textValue must be("noidea")
  }

  "A TemperatureSensor" must "switch to initialized status when required" in {
    val tested = new BadTemperatureSensor(30l)

    tested.initialize

    tested.initialized must be(true)
  }


  "TheDsl" must "allow to construct a TemperatureSensor from a Double" in {
    val builder = SensorCollectionDsl.start

    builder.withTemperatureSensor(3.0)
  }

  it must "not allow to construct a TemperatureSensor from a String" in {
    val builder = SensorCollectionDsl.start

    //builder.withTemperatureSensor("3.0")
    ???
  }



  "A SolarIrradianceSensor must cap geographically incorrect values as invalid" must "output sane data" in {
    val valueCheck = List((90.0, 160.0), (90.0, 380.0), (52.0, 120.0))

    for ((latitude, maxValue) <- valueCheck) {
        new SolarIrradianceSensorSensor(7, latitude).value.get must be < maxValue
    }
  }


  "A SensorData display" must "correctly sort a list" in {
    val testList = List((0l, 1.0),(1l, -1.0), (1l, 0.0)).map(SensorDataPoint.tupled)

    new SensorDataDisplay(testList).sorted //no exception
  }

}
