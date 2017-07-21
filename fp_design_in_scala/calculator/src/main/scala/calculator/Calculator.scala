package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
        def evalSignal(name: String, exprSig: Signal[Expr]) = {
          (name, Signal {eval(exprSig(), namedExpressions)})
        }
        namedExpressions map {case (name, exprSig) => evalSignal(name, exprSig)}
  }

  def eval(expr: Expr, refs: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(name) => {
        val refExpr = getReferenceExpr(name, refs)
        eval(refExpr, refs - name)
      }
      case Plus(lExpr, rExpr) => eval(lExpr, refs) + eval(rExpr, refs)
      case Minus(lExpr, rExpr) => eval(lExpr, refs) - eval(rExpr, refs)
      case Times(lExpr, rExpr) => eval(lExpr, refs) * eval(rExpr, refs)
      case Divide(lExpr, rExpr) => eval(lExpr, refs) / eval(rExpr, refs)
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      refs: Map[String, Signal[Expr]]) = {
    refs.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
