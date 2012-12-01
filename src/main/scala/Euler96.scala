import PuzzleReader._
import Solver.solveIt

object Euler96 {

  def main(args: Array[String]) {
    (for {
      board <- readPuzzle(new java.io.File("/Users/eolander/scala-sudoku/src/main/resouces/sudoku.txt"))//.drop(22)

    } yield solveIt(board)).toList

  }

}
