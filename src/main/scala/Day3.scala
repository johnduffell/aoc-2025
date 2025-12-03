object Day3 extends App {

  val data = """987654321111111
               |811111111111119
               |234234234234278
               |818181911112111""".stripMargin

  private val lines: List[Array[Int]] =
    data
      .linesIterator
      .toList
      .map(line => line.map(_.toString.toInt).toArray)

  val result1 =
    lines
      .map { line =>
        val max = line.init.max
        val remainingLine = line.drop(line.indexOf(max) + 1)
        val maxAfter = remainingLine.max
        (max * 10) + maxAfter
      }
      .sum

  println(result1)

  val result2 =
    lines
      .map { line =>
        (11 to 0 by -1).foldLeft((line, 0L)) { case ((line, accu), charsToRetain) =>
          val max = line.dropRight(charsToRetain).max
          val remainingLine = line.drop(line.indexOf(max) + 1)
          (remainingLine, max + (accu * 10))
        }._2
      }
      .sum

  println(result2)
}
