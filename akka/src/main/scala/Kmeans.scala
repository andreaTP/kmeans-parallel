
import akka.actor._

import KmeansMsgs._
import CentMsgs._
import PointMsgs._

object KmeansMsgs {
  case object Inc
  case class NewCentroid(p: Point)
}

case class KmeansActor(n: Int, iter: Int, xs: List[Point], printResult: Boolean = false) extends Actor {

  val cents = (for (i <- 0 until n) yield context.actorOf(Props[CentActor])).toList
  val pas = for (p <- xs) yield context.actorOf(Props(PointActor(p, cents)))

  def receive = start()

  def start(i: Int = 0, centroids: List[Point] = xs.take(n)): Receive = {
    if (i >= iter) {
      if (printResult) {
        println("Final centroids are:")
        centroids.foreach(x => println(x))
      }

      context.parent ! End
    }
    pas.foreach(_ ! UpdateClosest(centroids))
    step(i, 0, centroids)
  }

  def step(i: Int, pupdated: Int, centroids: List[Point]): Receive = {
    if (pupdated >= xs.size) {
      cents.foreach(_ ! Assign)
      updateCentroids(i)
    } else {
      case Inc =>
        context.become(step(i, pupdated + 1, centroids))
    }
  }

  def updateCentroids(i: Int, centroids: List[Point] = List()): Receive = {
    if (centroids.size >= n) {
      start(i + 1, centroids)
    } else {
      case NewCentroid(c: Point) =>
        context.become(updateCentroids(i, centroids :+ c))
    }
  }

}

object CentMsgs {
  case class Add(p: Point)
  case object Assign
}

case class CentActor() extends Actor {

  def receive = accumulate(List())

  def average(xs: List[Point]) = xs.reduce(_ + _) / xs.size  

  def accumulate(points: List[Point]): Receive = {
    case Add(p) =>
      context.become(accumulate(points :+ p))
      context.parent ! Inc
    case Assign =>
      context.parent ! NewCentroid(average(points))
      context.become(accumulate(List()))
  }

}

object PointMsgs {
  case class UpdateClosest(centroids: List[Point])
}

case class PointActor(p: Point, centActs: List[ActorRef]) extends Actor {

  def dist(y: Point) = (p - y).modulus

  def closest(choices: List[Point]) =
    choices.zipWithIndex.minBy{ x => dist(x._1) }._2

  def receive = {
    case UpdateClosest(centroids) =>
      centActs(closest(centroids)) ! Add(p)
  }

}