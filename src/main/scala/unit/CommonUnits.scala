package moira.unit

object CommonUnits {
  val meter = SIUnit(1, SIDim(m=1))
  val kilogram = SIUnit(1, SIDim(kg=1))
  val second = SIUnit(1, SIDim(s=1))
  val ampere = SIUnit(1, SIDim(a=1))
  val kelvin = SIUnit(1, SIDim(k=1))
  val candela = SIUnit(1, SIDim(cd=1))
  val mole = SIUnit(1, SIDim(mol=1))

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

  val names = Map(
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

  implicit def doubleToUnit(value: Double): SIUnit = SIUnit(value, SIDim())
}

