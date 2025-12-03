import scala.io.Source

def datal(num: Int): String = {
  Source.fromResource("day" + num).getLines().mkString("\n")
}

def p[U](f: => U): U = {
  (0 until 100).foreach(_ => f)
  val start = System.nanoTime()
  val u = f
  val time = ((System.nanoTime() - start)/1000)/1000d
  println(s"$time ms: $u")
  u
}