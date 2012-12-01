import java.io.File
import Solver.Board

object PuzzleReader {

  val grid6 =
    """100920000
      |524010000
      |000000070
      |050008102
      |000000000
      |402700090
      |060000000
      |000030945
      |000071006
    """.stripMargin.trim.split("\n").iterator

  val grid7 = """043980250
                |600425000
                |200001094
                |900004070
                |300608000
                |410209003
                |820500009
                |000000005
                |534890710""".stripMargin.trim.split("\n").iterator

  val grid48 =
  """001007090
    |590080001
    |030000080
    |000005800
    |050060020
    |004100000
    |080000030
    |100020079
    |020700400""".stripMargin.trim.split("\n").iterator

  val grid50 =
  """300200000
    |000107000
    |706030500
    |070009080
    |900020004
    |010800050
    |009040301
    |000702000
    |000008006""".stripMargin.trim.split("\n").iterator


  def positionList(list: Iterator[String]) = {
    (for {
      (s, r) <- list.zipWithIndex
      (v, c) <- s.zipWithIndex filterNot(_._1 == '0')
    } yield (r, c, v.asDigit)).toList
  }

  def readPuzzle(file: File): Stream[Board] = {
    def buildBoard(lines: Iterator[String]): Stream[Board] = {
      if (lines.isEmpty) Stream.empty
      else {
        println(lines.next)
      //  lines.drop(1)
        Solver.initBoard(Solver.initialBoard, positionList(lines.take(9))) #:: buildBoard(lines)
      }
    }
    buildBoard(io.Source.fromFile(file).getLines())
  }
}
