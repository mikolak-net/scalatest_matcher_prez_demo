import org.scalactic.Equality
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, MustMatchers}

class GallantTest extends FlatSpec with MustMatchers with PropertyChecks {


  case class DummySensor(override val name: String, override val id: Long, valueFunc: () => Double) extends Sensor(name, id) {

    var initialized = false

    def value = Some(valueFunc())

    def initialize = {initialized = true}

  }

  "A Temperature sensor" must "store its ID" in {
    val id = 42l
    val tested = new TemperatureSensor(id)

    tested.id must be(id)
    tested.id mustBe id //no parens
    tested.id must equal(id)
    tested.id mustEqual id //no parens
  }

  it must "the same as another sensor with the same id" in {
    implicit val sensorEq = SensorEquality

    val id = 7l
    val tested: Sensor = new TemperatureSensor(id) //won't work without type ascription

    tested must equal(DummySensor("blah", id, () => 0))
  }

  "A SingleSensorCollector" must "be the same reference as" in {
    val sensor = DummySensor("blah", 7, () => 10)
    val tested = new SingleSensorCollector(sensor)

    tested.sensors.head must be theSameInstanceAs sensor
  }


  it must "not throw an exception when adding a sensor" in {
    noException must be thrownBy  {
      new SingleSensorCollector(new DummySensor("blah", 7, () => 10))
    }
  }

  it must "throw an IllegalArgumentException when attempting to add a null sensor" in {
    an [IllegalArgumentException] must be thrownBy {
      new SingleSensorCollector(null)
    }
  }

  "A TemperatureSensor's value" must "be within a range of +-3 when freshly initialized" in {
    val tested = new TemperatureSensor(0l)
    tested.initialize
    tested.value.get must be (0.0 +- 3.0)
  }

  "A MultiSensorCollection" must "produce a complete collection" in {
    val testData = List(DummySensor("blah", 10, () => 0), DummySensor("blah", 7, () => 0), DummySensor("blah", 8, () => 0))

    val tested = new MultiSensorCollector(List.empty)
    testData.foreach(tested.addSensor)

    tested.sensors must contain theSameElementsAs(testData)
  }

  it must "add sensors in order" in {
    val testData = List(DummySensor("blah", 10, () => 0), DummySensor("blah", 7, () => 0), DummySensor("blah", 8, () => 0))

    val tested = new MultiSensorCollector(List.empty)
    testData.foreach(tested.addSensor)

    tested.sensors must contain theSameElementsInOrderAs(testData)
  }

  it must "return an empty list when provided with an empty seed" in {
    new MultiSensorCollector(List.empty).sensors must be('empty)
  }

  it must "remove an element provided" in {
    val testData = List(DummySensor("blah", 10, () => 0), DummySensor("blah", 7, () => 0), DummySensor("blah", 8, () => 0))
    val deletedItem = testData(1)

    val tested = new MultiSensorCollector(List.empty)
    testData.foreach(tested.addSensor)

    tested.removeSensor(deletedItem)

    tested.sensors must not contain deletedItem
  }

  it must "return a Java list with sth" in {
    val testData = List(DummySensor("blah", 10, () => 0), DummySensor("blah", 7, () => 0), DummySensor("blah", 8, () => 0))

    val tested = new MultiSensorCollector(List.empty)
    testData.foreach(tested.addSensor)

    tested.legacySensors must contain theSameElementsInOrderAs(testData)
  }

  it must "return the same type of a sensor as searched" in {
    val searchedName = "something"
    val tested = new MultiSensorCollector(List(DummySensor(searchedName, 10, () => 0)))

    import org.scalatest.OptionValues._
    tested.findByName(searchedName).value.name must be(searchedName)
  }


  it must "return the first matching sensors as searched" in {
    val searchedName = "something"
    val firstId = 10l
    val tested = new MultiSensorCollector(List(DummySensor(searchedName, firstId, () => 0),DummySensor(searchedName, 11, () => 0)))

    import org.scalatest.OptionValues._
    tested.findByName(searchedName).value.name must have (
      'name (searchedName),
      'id (firstId)
    )
    //alternatively
    tested.findByName(searchedName).value must haveId(firstId)
  }


  "A TemperatureSensor's text output" must "have be prefixed with the name of the sensor" in {
    new TemperatureSensor(0).textValue must startWith("Temp")
  }

  it must "output a sensible start value in text" in {
    new TemperatureSensor(30).textValue must fullyMatch regex (raw"Temp: (\d+)K" withGroup("0"))
  }

  "A TemperatureSensor" must "switch to initialized status when required" in {
    val tested = new BadTemperatureSensor(30l)

    tested.initialize

    tested must be an 'initialized
  }


  "TheDsl" must "allow to construct a TemperatureSensor from a Double" in {
    val builder = SensorCollectionDsl.start

    "builder.withTemperatureSensor(3.0)" must compile
  }

  it must "not allow to construct a TemperatureSensor from a String" in {
    val builder = SensorCollectionDsl.start

    """builder.withTemperatureSensor("3.0")""" mustNot typeCheck

    """builder.withTemperatureSensor("3.0")""" mustNot compile
  }



  "A SolarIrradianceSensor must cap geographically incorrect values as invalid" must "output sane data" in {
    val valueCheck = Table(("Latitude", "Max value"), (90.0, 160.0), (90.0, 380.0), (52.0, 120.0))

    forAll(valueCheck) { (latitude, maxValue) => {
        new SolarIrradianceSensorSensor(7, latitude).value.get must be < maxValue
      }
    }
  }


  "A SensorData display" must "correctly sort a list" in {
    import org.scalacheck.Gen._
    val dataPointGen = zip(posNum[Long], posNum[Double]).map(SensorDataPoint.tupled)
    val pointLists = listOf(dataPointGen)

    forAll(pointLists) { (list) => {
        noException must be thrownBy  {
          new SensorDataDisplay(list).sorted
        }
      }
    }
  }


  //Util stuff

  class HaveIdMatcher(expectedId: Long) extends Matcher[Sensor] {
    override def apply(left: Sensor) = {
      MatchResult(
        left.id.equals(expectedId),
        s"Sensor $left has a different ID than $expectedId",
        s"Sensor $left has the ID $expectedId"
      )
    }
  }

  def haveId(expectedId: Long) = new HaveIdMatcher(expectedId)



  object SensorEquality extends Equality[Sensor] {
    def areEqual(a: Sensor, b: Any) = b match {
      case other: Sensor => other.id == a.id
      case _ => false
    }

  }

}
