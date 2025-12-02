import scala.io.Source

def datal(num: Int): String = {
  Source.fromResource("day" + num).getLines().mkString("\n")
}

def p[U](f: => U): U = {
  val start = System.currentTimeMillis()
  val u = f
  val time = System.currentTimeMillis() - start
  println(s"$time ms: $u")
  u
}