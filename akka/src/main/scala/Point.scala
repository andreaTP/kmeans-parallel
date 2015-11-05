
case class Point(x: Double, y: Double) {
  def /(k: Double): Point = Point(x / k, y / k)

  def +(p2: Point) = Point((x + p2.x), (y + p2.y))
  def -(p2: Point) = Point((x - p2.x), (y - p2.y))

  def sq(x: Double) = x*x

  import math.sqrt
  def modulus = sqrt(sq(x) + sq(y))
}