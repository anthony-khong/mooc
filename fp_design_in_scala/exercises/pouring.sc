type Capacity = Vector[Int]
type State = Vector[Int]

class Pouring(capacity: Capacity) {
  val glasses = 0 until capacity.size
  val initialState = capacity map (_ => 0)

  trait Move { def change(state: State): State }
  case class Empty(glass: Int) extends Move {
    def change(state: State): State = state updated (glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    def change(state: State): State = state updated (glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State): State = {
      val amount = state(from) min (capacity(to) - state(to))
      state updated (from, state(from) - amount) updated (to, state(to) + amount)
    }
  }

  val moves = {
    (for (glass <- glasses) yield Empty(glass)) ++
    (for (glass <- glasses) yield Fill(glass)) ++
    (for (x <- glasses; y <- glasses if x != y) yield Pour(x, y))
  }

  class Path(history: List[Move]) {
    def endState: State = (history foldRight initialState) (_ change _)
    override def toString = {
      initialState + " => " + (history.reverse mkString "->") + " => " + endState
    }
    def extend(move: Move) = new Path(move :: history)
  }

  val initialPath = new Path(Nil)
  def nextLvlPaths(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] = {
    if (paths.isEmpty) Stream()
    else {
      val nextPaths = for {
        path <- paths
        nextPath <- moves map (path extend _)
        if !(explored contains nextPath.endState)
      } yield nextPath
      val addedStates = nextPaths map (_.endState)
      paths #:: nextLvlPaths(nextPaths, explored ++ addedStates)
    }
  }

  val pathSets = nextLvlPaths(Set(initialPath), Set())

  def solutions(target: Int): Stream[Path] = for {
    pathSet <- pathSets
    path <- pathSet
    if path.endState contains target
  } yield path

}

@main
def main(): Unit = {
  val problem = new Pouring(Vector(4, 9))
  println(problem.moves)
  println(problem.solutions(6))
}
