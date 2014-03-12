package moira.expression

// function call
case class Funcall(name: String, args: Seq[Expr]) extends Expr {
  def bind(bindings: Bindings) = {
    Funcall(name, args.map(_.bind(bindings)))
  }

  def unify(x: String, ys: Seq[String]): Expr = {
    Funcall(name, args.map(_.unify(x, ys)))
  }

  lazy val vars = args.foldLeft(Set[String]()) { (vs, e) => vs ++ e.vars }
  lazy val simplified = Funcall(name, args.map(_.simplified))

}
