object Day1 {
  val data =
    """L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"""


  val movements: List[Int] = data.linesIterator.toList.map(_.splitAt(1) match {
    case ("L", num) => num.toInt * -1
    case ("R", n) => n.toInt
  })

  object partA  {
    val positions = movements.scanLeft(50) { (curPos, nextMovement) =>
      (curPos + nextMovement + 10000) % 100
    }

    val result = positions.count(_ == 0)
  }

  object partB {

    def oneMove(curPos: Int, nextMovement: Int): (Int, Int) = {
      val endPoint = curPos + nextMovement
      if (endPoint == 0) {
        (0, 1)
      } else if (endPoint < 0) {
        if (endPoint % 100 == 0) {
          val passes = (endPoint / -100) + 1
          val actualPosition = 0
          (actualPosition, passes)
        } else {
          val passes = (endPoint / -100) + 1
          val actualPosition = endPoint + (passes * 100)
          (actualPosition, passes - (if (curPos == 0) 1 else 0))
        }
      } else if (endPoint % 100 == 0) {
        val passes = endPoint / 100
        val actualPosition = 0
        (actualPosition, passes)
      } else {
        val passes = endPoint / 100
        val actualPosition = endPoint - (passes * 100)
        (actualPosition, passes)
      }
    }

    val (_, result) = movements.foldLeft((50, 0)) {
      case ((curPos, passesSoFar), nextMovement) =>
        val (newPosition: Int, actualPasses: Int) = oneMove(curPos, nextMovement)
        (newPosition, passesSoFar + actualPasses)
    }

    val test =
      List(
        // off zero
        (0, 10, 10, 0),
        (0, -10, 90, 0),
        // on to zero
        (10, -10, 0, 1),
        (90, 10, 0, 1),
        // pass zero once
        (10, -20, 90, 1),
        (90, 20, 10, 1),
        // **wrap around**
        // off zero
        (0, 110, 10, 1),
        (0, -110, 90, 1),
        // on to zero
        (10, -110, 0, 2),
        (90, 110, 0, 2),
        // pass zero twice
        (10, -120, 90, 2),
        (90, 120, 10, 2),
      ).map { (curPos: Int, nextMovement: Int, expectedPos: Int, expectedPasses: Int) =>
        val (newPosition: Int, actualPasses: Int) = partB.oneMove(curPos, nextMovement)
        val result = s"oneMove(curPos = $curPos, nextMovement = $nextMovement) = (newPosition = $newPosition, actualPasses = $actualPasses)"
        if (newPosition != expectedPos) throw new RuntimeException(result + s"\nwrong position should be $expectedPos")
        if (actualPasses != expectedPasses) throw new RuntimeException(result + s"\nwrong number of passes should be $expectedPasses")
      }

  }

  @main
  def day1() = {
    println(partA.result)
    println(partB.result)
  }
}