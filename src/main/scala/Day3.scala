import scala.annotation.tailrec

object Day3 extends App {

  val data = """987654321111111
               |811111111111119
               |234234234234278
               |818181911112111""".stripMargin

  val lines: List[List[Int]] =
    datal(3)
      .linesIterator
      .toList
      .map(line => line.map(_.asDigit).toList)

  def result1 =
    lines
      .map { line =>
        val max = line.init.max
        val remainingLine = line.drop(line.indexOf(max) + 1)
        val maxAfter = remainingLine.max
        (max * 10) + maxAfter
      }
      .sum

  p(result1)

  def result2 =
    lines
      .map { line =>
        (11 to 0 by -1).foldLeft((line, 0L)) { case ((line, accu), charsToRetain) =>
          val max = line.dropRight(charsToRetain).max
          val remainingLine = line.drop(line.indexOf(max) + 1)
          (remainingLine, max + (accu * 10))
        }._2
      }
      .sum

  p(result2)


  @tailrec
  def maxOfFirstSection(line: List[Int], amountToTake: Int, accu: Int = 0): Int =
    line match {
      case head :: tail if amountToTake > 0 =>
        maxOfFirstSection(tail, amountToTake - 1, Math.max(accu, head))
      case _ =>
        accu
    }

  def result2Optimised =
    lines
      .map { line =>
        (11 to 0 by -1).foldLeft((line, 0L)) { case ((line, accu), charsToRetain) =>
          val rightToIgnore = line.length - charsToRetain
          val max = maxOfFirstSection(line, rightToIgnore)
          val remainingLine = line.drop(line.indexOf(max) + 1)
          (remainingLine, max + (accu * 10))
        }._2
      }
      .sum

  p(result2Optimised)
}
