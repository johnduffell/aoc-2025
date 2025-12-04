import scala.annotation.tailrec

object Day4 extends App {

  val data = """..@@.@@@@.
               |@@@.@.@.@@
               |@@@@@.@.@@
               |@.@@@@..@.
               |@@.@@@@.@@
               |.@@@@@@@.@
               |.@.@.@.@@@
               |@.@@@.@@@@
               |.@@@@@@@@.
               |@.@.@@@.@.""".stripMargin

  val grid: List[List[Boolean]] =
    data
      .linesIterator
      .toList
      .map(_.toList.map(_ == '@'))

  case class GroupState(numberFilled: Int, isRoll: Boolean)

  val blankRow = List.fill(grid.head.length)(false)

  def buildGridWithAdjacents(grid: List[List[Boolean]]): List[List[GroupState]] = {
    val padded: List[List[Boolean]] = wrapList(blankRow, grid).map { row =>
      wrapList(false, row)
    }
    val groups: List[List[GroupState]] =
      padded
        .map { row =>
          row.
            sliding(3)
            .toList
            .map { case l @ List(a, b, c) =>
              GroupState(l.count(identity), b)
            }
        }
        .sliding(3)
        .toList
        .map { case List(a, b, c) =>
          a.zip(b).zip(c)
          .map { case ((a, b), c) =>
            GroupState(a.numberFilled + b.numberFilled + c.numberFilled, b.isRoll)
          }
        }
    groups
  }

  private def wrapList[A](blankRow: A, grid: List[A]): List[A] =
    blankRow :: (grid :+ blankRow)

  private val initialGridWithAdjacents = buildGridWithAdjacents(grid)

  val result1 =
    (for {
      row <- initialGridWithAdjacents
      numMovableForRow <- row.collect { case GroupState(total, true) if total < 5 => 1 }
    } yield numMovableForRow).sum

  println(result1)

  val initialRolls = getNumRolls(grid)

  def getNumRolls(grid: List[List[Boolean]]): Int = {
    grid.map(_.count(identity)).sum
  }

  def updateCells(gridWithAdjacents: List[List[GroupState]]) =
    gridWithAdjacents.map(_.map {
      case GroupState(total, true) if total < 5 => false
      case GroupState(_, isRoll) => isRoll
    })

  @tailrec
  def clearMaxRolls(gridWithAdjacents: List[List[GroupState]], numRolls: Int): Int = {
    val res = updateCells(gridWithAdjacents)
    val newNum = getNumRolls(res)
    if (newNum == numRolls) initialRolls - numRolls
    else clearMaxRolls(buildGridWithAdjacents(res), newNum)
  }

  val result2 = clearMaxRolls(initialGridWithAdjacents, initialRolls)

  println(result2)


}
