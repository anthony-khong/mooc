class Poly(initTerms: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  val terms = initTerms withDefaultValue 0.0

  def + (other: Poly) = new Poly(terms ++ (other.terms map addTerm))
  def addTerm(term: (Int, Double)): (Int, Double) = {
    val (power, coeff) = term
    power -> (terms(power) + coeff)
  }

  def foldAdd(other: Poly) = new Poly((other.terms foldLeft terms)(foldAcc))
  def foldAcc(terms: Map[Int, Double], newTerm: (Int, Double)): Map[Int, Double] = {
    val (power, coeff) = newTerm
    val updatedCoeff = (coeff + terms(power))
    terms + (power -> updatedCoeff)
  }

  override def toString = {
    val sortedPairs = terms.toList.sorted
    sortedPairs map {case (p, c) => c + "x^" + p} mkString " + "
  }

}

val poly0 = new Poly(0 -> 2, 1 -> 2, 3 -> 10)
val poly1 = new Poly(1 -> -1, 3 -> 1, 10 -> 3)
println(poly0)
println(poly1)
println(poly0 + poly1)
println(poly0 foldAdd poly1)
