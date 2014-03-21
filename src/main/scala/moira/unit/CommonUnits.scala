package moira.unit

object CommonUnits {
  // no dimension
  val one = SIUnit(1, CommonDims.NODIM)

  // basic units
  val meter = SIUnit(1, CommonDims.LENGTH)
  val kilogram = SIUnit(1, CommonDims.MASS)
  val second = SIUnit(1, CommonDims.TIME)
  val ampere = SIUnit(1, CommonDims.ELECTRIC_CURRENT)
  val kelvin = SIUnit(1, CommonDims.TEMPERATURE)
  val candela = SIUnit(1, CommonDims.LUMINOUS_INTENSITY)
  val mole = SIUnit(1, CommonDims.AMOUNT_OF_SUBSTANCE)

  // prefixes
  val kilo = 1000.0
  val milli = 0.001

  // mass
  val gram = milli * kilogram
  val milligram = milli * gram

  // length
  val kilometer = kilo * meter
  val millimeter = milli * meter

  // area
  val meter2 = meter ** 2

  // volume
  val meter3 = meter ** 3

  // velocity
  val meterPerSecond = meter / second

  // acceleration
  val meterPerSecond2 = meter / (second ** 2)

  // force
  val newton = kilogram * meterPerSecond2

  // energy
  val joule = newton * meter

  // power
  val watt = joule / second

  // pressure
  val pascal = newton / (meter ** 2)

  // electricity
  val coulomb = ampere * second
  val volt = watt / ampere
  val farad = coulomb / volt
  val ohm = volt / ampere

  // known units
  val nameToUnit: Map[String, SIUnit] = Map(
    "" -> one,
    "m" -> meter,
    "kg" -> kilogram,
    "s" -> second,
    "A" -> ampere,
    "K" -> kelvin,
    "cd" -> candela,
    "mol" -> mole,

    "g" -> gram,
    "mg" -> milligram,

    "km" -> kilometer,
    "mm" -> millimeter,

    "m2" -> meter2,
    "m3" -> meter3,

    //"m/s" -> meterPerSecond,
    //"m/s2" -> meterPerSecond2,

    "N" -> newton,
    "J" -> joule,
    "W" -> watt,
    "Pa" -> pascal,

    "C" -> coulomb,
    "V" -> volt,
    "F" -> farad
  )
  val unitToName: Map[SIUnit, String] = nameToUnit.map(_.swap)

  // Parses /str/ into /SIUnit/.
  def parseUnit(str: String): Option[SIUnit] = {
    // look up /nameToUnit/
    nameToUnit.get(str) match {
      case Some(unit) => Some(unit)
      case None => {
        // try to parse /str/
        try {
          require(str.size >= 2, s"$str is too short.")
          require(str.head == '<' && str.last == '>',
            s"$str isn't enclosed by brackets.")
          require(str.count(_=='*') == 1,
            s"$str contains ${str.count(_=='*')} asterisks, not 1.")

          val (fStr, dimStr) = {
            val s = str.substring(1, str.size-1).split("\\*", 2)
            assert(s.size == 2)
            (s(0), s(1))
          }

          // factor
          val f = fStr.toDouble

          // create /SIUnit/ if /dimStr/ is parsable
          SIDim.fromStr(dimStr).map(SIUnit(f, _))
        } catch {
          case e: Exception => throw e//None
        }
      }
    }
  }

  // Returns pairs of known units and their names that are of the dimension /dim/.
  def unitNames(dim: SIDim): Seq[(String, SIUnit)] = {
    val knownUnits = (nameToUnit filter {
      case (n, u) => u.dim == dim
    }).toSeq

    // append default unit so that dim without known units have at least 1 unit registered.
    val defaultUnit = SIUnit(1.0, dim)
    val defaultUnitName = defaultUnit match {
      case SIUnit(factor, dim) => s"<$factor*$dim>"
    }

    knownUnits :+ (defaultUnitName, defaultUnit)
  }

  implicit def doubleToUnit(value: Double): SIUnit = SIUnit(value, SIDim())
}

