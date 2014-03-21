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
  val nameToDim: Map[String, SIDim] = Map(
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

  val dimToName: Map[SIDim, String] = nameToDim.map(_.swap)
}

