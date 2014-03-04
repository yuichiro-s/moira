package moira.constraint

import moira.unit.SIDim
import moira.unit.PhysicalQuantity

// parameter
case class Parameter(id: Int, dim: SIDim, lower: PhysicalQuantity, upper: PhysicalQuantity) {
  // dimensions must be consistent
  require(lower.unit.dim == dim)
  require(upper.unit.dim == dim)
}

