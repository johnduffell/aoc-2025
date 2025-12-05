import scala.annotation.tailrec
import scala.collection.immutable.NumericRange

object Day5 extends App {

  val data = """3-5
               |10-14
               |16-20
               |12-18
               |
               |1
               |5
               |8
               |11
               |17
               |32""".stripMargin

  val (freshRanges, ingredientIds) =
    data
      .split("\n\n") match {
      case Array(freshRanges, ingredientIds) =>
        val parsedFreshRanges =
          freshRanges
            .linesIterator
            .toList
            .map { range =>
              range
                .split('-') match {
                case Array(start, end) =>
                  start.toLong to end.toLong
              }
            }
        val parsedIngredientIds =
          ingredientIds
            .linesIterator
            .toList
            .map(_.toLong)
        (parsedFreshRanges, parsedIngredientIds)
    }

  @tailrec
  def countFreshIngredients0(remainingIngredients: List[Long], remainingLowestFinishersFirst: List[NumericRange.Inclusive[Long]], stillToCheckStart: List[Long] = List.empty, accu: Int = 0): Int = {
    (remainingIngredients, remainingLowestFinishersFirst, stillToCheckStart) match {
      case (_, Nil, _) =>
        // finished, no more ranges.
        // If any are left on the lists, they are all spoiled - too big are still on remainingIngredients and too small and gaps are on stillToCheckStart
        accu

      case (_, lowestFinisher :: restLowestFinishersFirst, highestIngredient :: restStillToCheckStart) if highestIngredient >= lowestFinisher.start =>
        // looking at a range that finishes later than our ingredient, now found it starts before before our stored ingredient
        // we've got to a range starts earlier than any previous one and it caught our stored ingredient
        countFreshIngredients0(remainingIngredients, remainingLowestFinishersFirst, restStillToCheckStart, 1 + accu)

      case (Nil, lowestFinisher :: restLowestFinishersFirst, _) =>
        // only queued ingredients to deal with now but they are all lower than the bottom of the range
        // get rid of the range
        countFreshIngredients0(remainingIngredients, restLowestFinishersFirst, stillToCheckStart, accu)

      case (ingredient :: restIngredients, lowestFinisher :: restLowestFinishersFirst, _) if ingredient > lowestFinisher.end =>
        // range is totally before the current ingredient
        // get rid of the range
        countFreshIngredients0(remainingIngredients, restLowestFinishersFirst, stillToCheckStart, accu)

      case (ingredient :: restIngredients, _, _) =>// if ingredient < lowestFinisher.start =>
        // ingredient is not off the high end of the range
        // save it to check against the beginning of the ranges
        countFreshIngredients0(restIngredients, remainingLowestFinishersFirst, ingredient :: stillToCheckStart, accu)

    }
  }

  val result1 =
    countFreshIngredients0(ingredientIds.sorted, freshRanges.sortBy(_.end))

  println(result1)

  def totalAllRanges(lowestFinishersFirst: List[NumericRange.Inclusive[Long]]) =
    List.unfold(lowestFinishersFirst) {
      case l @ h :: t =>
        val endingAfterUsSortedByStart = l.sortBy(_.start)
        val lastContigEnd = endingAfterUsSortedByStart.foldLeft(h.end) { (endSoFar, nextRange) => if (nextRange.start <= endSoFar) Math.max(nextRange.end, endSoFar) else endSoFar }
        val widened = (endingAfterUsSortedByStart.head.start, lastContigEnd)
        Some((widened, t))
      case Nil => None
    }
      .groupMap(_._2)(_._1)
      .view
      .mapValues(_.head)
      .map(_ - _ + 1)
      .sum

  val result2 = totalAllRanges(freshRanges.sortBy(_.end))

  println(result2)

}
