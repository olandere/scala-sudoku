import annotation.tailrec

object Solver {

  trait Cell {
    def isSolved: Boolean

    def row: Int

    def col: Int

    def value: Int

    def values: Set[Int]

    def remove(v: Int): Cell

    def remove(vs: Seq[Int]): Cell

    def display: String
  }

  case class Solved(row: Int, col: Int, value: Int) extends Cell {
    def isSolved = true

    def remove(v: Int): Cell = {
      if (v == value) {
        throw new IllegalArgumentException("removing from solved cell")
      }
      else {
        this
      }
    }

    def values = Set(value)

    def remove(vs: Seq[Int]) = this

    def display = "S:" + value
  }

  case class Unsolved(row: Int, col: Int, values: Set[Int]) extends Cell {
    def isSolved = false

    def remove(v: Int): Cell = {
      if (values(v) && values.size == 2) {
        Solved(row, col, (values - v).head)
      }
      else {
        Unsolved(row, col, values - v)
      }
    }

    def remove(vs: Seq[Int]) = {
      if (vs.isEmpty) {
        this
      }
      else {
        remove(vs.head).remove(vs.tail)
      }
    }

    def value: Int = throw new IllegalArgumentException("value of unsolved cell")

    def display = "U:" + values.mkString(" ")
  }

  // functions for partitioning the board
  def subGrid(cell: Cell): Int = (cell.row / 3) * 3 + (cell.col / 3)

  def columns(cell: Cell): Int = cell.col

  def rows(cell: Cell): Int = cell.row

  val partFuncStream: Stream[Cell => Int] = subGrid _ #:: columns _ #:: rows _ #:: partFuncStream

  type Board = IndexedSeq[Cell]

  def boardIndex(cell: Cell): Int = cell.row * 9 + cell.col

  val initialBoard: Board = (0 until 81) map (x => new Unsolved(x / 9, x % 9, (1 to 9).toSet))

  var noProgressCount = 0

  //object Board {
  //val cells: Board = (0 until 81) map (x => new Unsolved(x/9, x%9, (0 until 9).toSet))

  def setCell(r: Int, c: Int, v: Int, b: Board): Board = {
    val cell = Solved(r, c, v)
    b updated(boardIndex(cell), cell)
  }

  def isSolved(board: Board) = board forall (c => c.isSolved)

  def isValidSolution(board: Board) = {
    def checkPartition(board: Board, f: Cell => Int) = partitionBoard(board, f).forall(cs => cs.foldLeft(Set[Int]())(_ + _.value) == (1 to 9).toSet)
    isSolved(board) && checkPartition(board, rows) && checkPartition(board, columns) && checkPartition(board, subGrid)
  }

  @tailrec
  def initBoard(b: Board, l: List[(Int, Int, Int)]): Board = {
    if (l.isEmpty) {
      b
    }
    else {
      val (r, c, v) = l.head
      val cell = Solved(r, c, v)
      initBoard(b updated(boardIndex(cell), cell), l.tail)
    }
  }

  @tailrec
  def updateBoard(board: Board, cells: Seq[Cell], values: Seq[Int]): Board = {
    if (cells.isEmpty) {
      board
    }
    else {
      updateBoard(board updated(boardIndex(cells.head), cells.head.remove(values)), cells.tail, values)
    }
  }

  val easypuzzle = initBoard(initialBoard, EasyPuzzle.board)
  val medpuzzle = initBoard(initialBoard, MedPuzzle.board)
  val hardpuzzle = initBoard(initialBoard, HardPuzzle.board)
  val evilpuzzle = initBoard(initialBoard, EvilPuzzle.board)

  def solve(board: Board): Board = {
    if (isSolved(board)) {
      board
    }
    else {
      val grids = board groupBy subGrid
      val x = for {
        i <- (0 until 9)
        (solved, unsolved) = grids(i) partition (c => c.isSolved)
        sv = solved map (c => c.value)
        nb <- updateBoard(board, unsolved, sv)
      } yield {
        nb
      } //board updated (boardIndex(cell), cell.remove(sv))
      println(x)
      board
    }
  }

  @tailrec
  def updatePartitions(board: Board, partitions: Seq[Seq[Cell]]): Board = {
    if (partitions.isEmpty) {
      board
    }
    else {
      val (solved, unsolved) = partitions.head.partition(c => c.isSolved)
      val sv = solved map (c => c.value)
      updatePartitions(updateBoard(board, unsolved, sv), partitions.tail)
    }
  }

  def printBoard(board: Board) = {
    @tailrec
    def printBoardHelper(board: Board) {
      if (!board.isEmpty) {
        println(board.take(9).map(_.display).mkString("|"))
        printBoardHelper(board.drop(9))
      }
    }
    printBoardHelper(board)
    board
  }

  def solve1(board: Board, f: Cell => Int): Board = {
    if (isSolved(board)) {
      board
    }
    else {
      updatePartitions(board, partitionBoard(board, f))
      //      val x = for {
      //        i <- (0 until 9)
      //        (solved, unsolved) = grids(i) partition (c => c.isSolved)
      //        sv = solved map (c => c.value)
      //        nb <- updateBoard(board, unsolved, sv)
      //      } yield nb //board updated (boardIndex(cell), cell.remove(sv))
      //  println(x)
      //    board
    }
  }

  def partitionBoard(board: Board, f: Cell => Int): Seq[Seq[Cell]] = {
    (board.groupBy(f)).map {
      case (k, v) => v
    }.toList
  }

  def findSingularValue(unsolved: Seq[Cell]): Option[(Int, Cell)] = {
    val cellsByValue = for {
          c <- unsolved
          v <- c.values}
       yield (v -> c)
    val singularCell = cellsByValue groupBy( _._1) filter{case (_, l) => l.size == 1}
    if (singularCell.isEmpty) None
    else singularCell.head match{case (k, v) => Some((k, v.head._2))}
  }

  def solveForSingleValues(board: Board, f: Cell => Int): Board = {
    def helper(board: Board, partitions: Seq[Seq[Cell]]):Board = {
      if (partitions.isEmpty) board
      else {
        val (_, unsolved) = partitions.head.partition(c => c.isSolved)
        if (unsolved.isEmpty) helper(board, partitions.tail) else {
        val newBoard = findSingularValue(unsolved).map{case (v, c) => setCell(c.row, c.col, v, board)}.getOrElse(board)
        helper(newBoard, partitions.tail)
        }
      }
    }

  //  println("sfsv: input")
//    printBoard(board)
    val partitions = partitionBoard(board, f)
//    println("sfsv: output")
    helper(board, partitions)
  }

  def eliminatePairs(board: Board, f: Cell => Int): Board = {

    def removePairValues(board: Board, pairCells: Iterable[Solver.Cell], unsolved: Seq[Solver.Cell]): Board = {
      if (pairCells.isEmpty) board else {
        val newBoard = updateBoard(board, unsolved filterNot(c => c.values == pairCells.head.values), pairCells.head.values.toSeq)
        removePairValues(newBoard, pairCells.tail, unsolved)
      }
    }

    def helper(board: Board, partitions: Seq[Seq[Cell]]):Board = {
      if (partitions.isEmpty) board
      else {
        val (_, unsolved) = partitions.head.partition(c => c.isSolved)
        if (unsolved.isEmpty || unsolved.size <= 2) helper(board, partitions.tail)
        else {
          val pairCells = unsolved groupBy (_.values) filter {case(v, l) => v.size == 2 && l.size == 2} map {
            case (_, l) => l.head
          }
          helper(removePairValues(board, pairCells, unsolved), partitions.tail)
        }
      }
    }
  //  println("elimPairs: ")
    val partitions = partitionBoard(board, f)
    helper(board, partitions)
  }

  def groupCellsByValue(unsolved: Seq[Cell]) = {
    (for {
      c <- unsolved
      v <- c.values
    } yield (v, c)).groupBy(_._1).map {
      case(k, v) => (k -> v.map{case (_, c) => c}.toSet)
    }
  }

  def simplifyCells(board: Board, f: Cell => Int): Board = {
    def helper(board: Board, partitions: Seq[Seq[Cell]]): Board = {
      def doUpdates(board: Board, cells: Map[Set[Cell], Seq[Int]]): Board = {
        if (cells.isEmpty) board
        else {
          val valuesToRemove = (cells.head._1.foldLeft(Set[Int]())(_ ++ _.values) -- cells.head._2).toSeq
       //   println("valuesToRemove: "+valuesToRemove)
          doUpdates(updateBoard(board, cells.head._1.toSeq, valuesToRemove), cells.tail)
        }
      }

      if (partitions.isEmpty) board
      else {
        val (_, unsolved) = partitions.head.partition(c => c.isSolved)
        if (unsolved.isEmpty) helper(board, partitions.tail)
        else {
          val cells = (for{(k, v) <- groupCellsByValue(unsolved).toList}
            yield(v, k)).groupBy(_._1).map{case (k, v) => (k, v.map(_._2))}.filter{case(k, v) => k.size == v.size && k.size > 1}
          helper(doUpdates(board, cells), partitions.tail)
        }
      }
    }

    val partitions = partitionBoard(board, f)
    helper(board, partitions)
  }

  def xxx(unsolved: Seq[Cell], f: Cell => Int) = {
    (for {
      c <- unsolved
      v <- c.values
    } yield (v, f(c))).groupBy(_._1).map {
      case(k, v) => (k -> v.map{case (_, col) => col}.toSet)
    }.filter{case(_, v) => v.size == 1}.map {case (k, v) => (k, v.head)}
  }

  // this searches subgrids for values that only occur in a single row or column of the subgrid.  If such a value
  // is found, the occurrences of it in the same row/column outside of the subgrid can be eliminated
  def subgridElimination(board: Board): Board = {

    def helper(board: Board, partitions: Seq[Seq[Cell]]):Board = {
      def eliminateFromRowOrCol(board : Board, valuesByColumn : Map[Int, Int], currentSubGrid: Int, p: Cell => Int): Board = {
        if (valuesByColumn.isEmpty) board
        else {
        val (v, rc) = valuesByColumn.head
        val cells = for {
          l <- partitionBoard(board, p)
          c <- l
          if p(c) == rc && !c.isSolved && subGrid(c) != currentSubGrid
        } yield (c)
          println("%d %d".format(v, rc)+cells)
          eliminateFromRowOrCol(updateBoard(board, cells, List(v)), valuesByColumn.tail, currentSubGrid, p)
        }
      }

      if (partitions.isEmpty) board
      else {
        val (_, unsolved) = partitions.head.partition(c => c.isSolved)
        val currentSubGrid = subGrid(partitions.head.head)
        val valuesByColumn = xxx(unsolved, columns)
        println(currentSubGrid+": byCol"+ valuesByColumn)
        val valuesByRow = xxx(unsolved, rows)
        println(currentSubGrid+": byRow"+ valuesByRow)
        helper(eliminateFromRowOrCol(eliminateFromRowOrCol(board, valuesByRow, currentSubGrid, rows),
                              valuesByColumn, currentSubGrid, columns), partitions.tail)
      }
    }
    helper(board, partitionBoard(board, subGrid))
  }

  // search rows/columns for values that only occur in a single subgrid.  Any that are found can be eliminated from the
  // other rows/columns within that subgrid
  def subGridElimination2(board: Board): Board = {

    def eliminateFromSubGrid(board: Board, valuesByColumn : Map[Int, Int], currentSubGrid: Int, p: Cell => Int):Board = {
      if (valuesByColumn.isEmpty) board
      else {
        val (v, rc) = valuesByColumn.head
        val cells = for {
          l <- partitionBoard(board, subGrid)
          c <- l
          if subGrid(c) == rc && !c.isSolved && p(c) != currentSubGrid
        } yield (c)
        println("%d %d".format(v, rc)+cells)
        eliminateFromSubGrid(updateBoard(board, cells, List(v)), valuesByColumn.tail, currentSubGrid, p)
      }
    }

    def helper(board: Board, partitions: Seq[Seq[Cell]], p: Cell => Int):Board = {
      if (partitions.isEmpty) board
      else {
        val (_, unsolved) = partitions.head.partition(c => c.isSolved)
        val valuesBySubGrid = xxx(unsolved, subGrid)
        val currRow = p(partitions.head.head)
        helper(eliminateFromSubGrid(board, valuesBySubGrid, currRow, p), partitions.tail, p)
      }
    }
    helper(helper(board, partitionBoard(board, rows), rows), partitionBoard(board, columns), columns)
  }

  def valueMap(start: Seq[Cell]) = {
    val(_, unsolved) = start.partition(c => c.isSolved)
    groupCellsByValue(unsolved)
  }

  def xWingElimination(board: Board):Board = {

    def findXWing(start: Seq[Cell], rest: Seq[Seq[Cell]]): Option[(Int, Int)] = {
      val currRow = start.head.row
      val map = valueMap(start).filter{case (_, v) => v.size == 2}

    }

    def helper(board: Board, partitions: Seq[Seq[Cell]], p: Cell => Int):Board = {
      if (partitions.isEmpty) board
      else {
        findXWing(partitions.head, partitions.tail)
      }
    }
    helper(board, partitionBoard(board, rows), rows)
  }

  @tailrec
  def refine(board: Board, f: Cell => Int): Board = {
    var newBoard = solve1(board, f)
    if (board == newBoard) {
//      if (f == subGrid)
//        subgridElimination(eliminatePairs(solveForSingleValues(board, f), f))
//      else
//      simplifyCells(eliminatePairs(solveForSingleValues(board, f), f), f)
      newBoard = solveForSingleValues(board, f)
      if (board != newBoard) newBoard
      else {
        newBoard = eliminatePairs(board, f)
        if (board != newBoard) newBoard
        else {
          simplifyCells(board, f)
        }
      }
    }
    else {
      refine(newBoard, f)
    }
  }

  def solveIt(board: Board): Board = {
    def helper(board: Board, fstream: Stream[Cell => Int]): Board = {
      printBoard(board)
      println
      if (isSolved(board)) {
        if (isValidSolution(board)) println("Valid solution")
        board
      }
      else {
        val newBoard = refine(board, fstream.head)
        if (newBoard == board) {
          println("no progress")
          noProgressCount += 1
          if (noProgressCount > 2) board else {
            val nb = subgridElimination(board)
            if (nb != board)
              helper(nb, fstream.tail)
            else helper(subGridElimination2(board), fstream.tail)
          }
        } else {
          noProgressCount = 0
          helper(newBoard, fstream.tail)
        }
      }
    }
    noProgressCount = 0
    helper(board, partFuncStream)
  }

  // }
}
