package moira.unit

object CommonDims {
  val NODIM = SIDim()

  val LENGTH = SIDim(m=1)
  val MASS = SIDim(kg=1)
  val TIME = SIDim(s=1)
  val ELECTRIC_CURRENT = SIDim(a=1)
  val TEMPERATURE = SIDim(k=1)
  val LUMINOUS_INTENSITY = SIDim(cd=1)
  val AMOUNT_OF_SUBSTANCE = SIDim(mol=1)

  val AREA = LENGTH ** 2
  val VOLUME = LENGTH ** 3

  val VELOCITY = LENGTH / TIME
  val ACCELERATION = VELOCITY / TIME

  val FREQUENCY = NODIM / TIME

  val DENSITY = MASS / VOLUME

  val FORCE = MASS * ACCELERATION
  val PRESSURE = FORCE / AREA
  val ENERGY = FORCE * LENGTH
  val POWER = ENERGY / TIME

  val ELECTRIC_CHARGE = ELECTRIC_CURRENT * TIME
  val VOLTAGE = ENERGY / ELECTRIC_CURRENT
  val CAPACITANCE = ELECTRIC_CHARGE / VOLTAGE
  val RESISTANCE = VOLTAGE / ELECTRIC_CURRENT

  // known dimensions
  val nameToDim = Map(
    "Dimensionless" -> NODIM,

    "Length" -> LENGTH,
    "Area" -> AREA,
    "Volume" -> VOLUME,

    "Velocity" -> VELOCITY,
    "Acceleration" -> ACCELERATION,

    "Time" -> TIME,
    "Frequency" -> FREQUENCY,

    "Mass" -> MASS,
    "Density" -> DENSITY,

    "Force" -> FORCE,
    "Pressure" -> PRESSURE,
    "Energy" -> ENERGY,
    "Power" -> POWER,

    "Electric Charge" -> ELECTRIC_CHARGE,
    "Voltage" -> VOLTAGE,
    "Capacitance" -> CAPACITANCE,
    "Resistance" -> RESISTANCE
  )
  val dimToName = nameToDim.map(_.swap)
}

object CommonUnits {
  val meter = SIUnit(1, CommonDims.LENGTH)
  val kilogram = SIUnit(1, CommonDims.MASS)
  val second = SIUnit(1, CommonDims.TIME)
  val ampere = SIUnit(1, CommonDims.ELECTRIC_CURRENT)
  val kelvin = SIUnit(1, CommonDims.TEMPERATURE)
  val candela = SIUnit(1, CommonDims.LUMINOUS_INTENSITY)
  val mole = SIUnit(1, CommonDims.AMOUNT_OF_SUBSTANCE)

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
  val newton = kilogram * meter / meterPerSecond2

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
  val nameToUnit = Map(
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
  val unitToName = nameToUnit.map(_.swap)

  // Returns pairs of known units and their names that are of the dimension /dim/.
  def unitNames(dim: SIDim): Seq[(String, SIUnit)] = {
    val defaultUnit = SIUnit(1.0, dim)
    val defaultUnitName = "<" + defaultUnit.toString + ">"
    Seq((defaultUnitName, defaultUnit)) ++
      nameToUnit filter { case (n, u) => u.dim == dim } toSeq
  }

  implicit def doubleToUnit(value: Double): SIUnit = SIUnit(value, SIDim())
}

