
import scala.io.Source
import org.json4s._
import org.json4s.jackson.JsonMethods._

import akka.actor._

object Main extends App {
  def readPoints(path: String) = {	
    val json = Source.fromFile(path).mkString
    implicit val formats = DefaultFormats

    parse(json).extract[List[List[Double]]] map { case List(a, b) => Point(a, b) }
  }

  val n = 10
  val iters = 15
  val iterations = 3

  val points = readPoints("../points.json")


  val system = ActorSystem("kmeans")

  for (i <- 1 to iterations) {
    val before = System.currentTimeMillis

    system.actorOf(Props(new Actor {
      context.actorOf(Props(KmeansActor(n, iters, points)))
      def receive = {
        case End =>
          val after = System.currentTimeMillis

          val time = (after- before) / iterations
          println(s"Average time is $time")
          
          if (i == iterations)
            system.shutdown
          else
            self ! PoisonPill
      }
      })
    )
  }
}

case object End
