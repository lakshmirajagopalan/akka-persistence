package game

import akka.actor.{ActorSystem, Props}
import scala.io.StdIn

object Play extends App {
  val system = ActorSystem("GamePersistence")
  val persistentActor = system.actorOf(Props(classOf[Game]), "play-persistent-actor")

  persistentActor ! "show"
  persistentActor ! "join jon"
  persistentActor ! "move jon Right"
  persistentActor ! "show"
  persistentActor ! "join sansa"
  persistentActor ! "move sansa Right"
  persistentActor ! "show"
  persistentActor ! "move sansa Up"
  persistentActor ! "show"
  persistentActor ! "move jon Right"
  persistentActor ! "move sansa Up"

  persistentActor ! "snap"
  persistentActor ! "join arya"
  persistentActor ! "move arya Up"
  persistentActor ! "show"
  StdIn.readLine()

  system.terminate()

  StdIn.readLine()

}
