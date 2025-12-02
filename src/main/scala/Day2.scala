object Day2 extends App {

  val data =
    """11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
      |1698522-1698528,446443-446449,38593856-38593862,565653-565659,
      |824824821-824824827,2121212118-2121212124""".stripMargin.linesIterator.mkString

  def idsToCheck =
    for {
      range <- data
        .split(',')
        .iterator
      (start, end) =
        range
          .split('-')
          .toList match {
          case start :: end :: Nil => (start, end)
        }
      res <- start.toLong to end.toLong
    } yield res

  val result1 = idsToCheck
    .filter { id =>
      val str = id.toString
      if (str.length % 2 == 1) false else {
        val (beginning, end) = str.splitAt(str.length / 2)
        beginning == end
      }
    }
    .sum

  println(result1)

  val result2 = idsToCheck
    .filter { id =>
      val str = id.toString
      val firstChar = str(0)
      val nextLocations = List.unfold(1) { idx =>
        Some(str.indexOf(firstChar, idx))
          .filter(_ != -1)
          .map(idx => (idx, idx + 1))
      }
      nextLocations.exists { nextLocation =>
        str
          .sliding(nextLocation, nextLocation)
          .toList match {
          case first :: rest => rest.forall(_ == first)
        }
      }
    }
    .sum

  println(result2)

}
