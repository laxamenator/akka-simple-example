package io.utopian.laxam.akka.simple

import akka.actor.{ Actor, ActorLogging, ActorRef, ActorSystem, Props }

object IntParser {
  final case class Parse(s: String)
  case object Print

  def hexProps(printer: ActorRef): Props = Props(new IntParser(16, printer))
  def decProps(printer: ActorRef): Props = Props(new IntParser(10, printer))
}

class IntParser(base: Int, printer: ActorRef) extends Actor {
  import IntParser._
  import Printer._

  var result = 0;
  var from = "";

  def receive = {
    case Parse(s) =>
      from = s
      result = Integer.parseInt(s, base)
    case Print =>
      printer ! Output(from, result)
  }
}

object Printer {
  def props: Props = Props[Printer]
  final case class Output(from: String, result: Int)
}

class Printer extends Actor with ActorLogging {
  import Printer._

  def receive = {
    case Output(from, result) =>
      log.info(s"${sender()}: $from => ${result.toString}")
  }
}

object AkkaSimpleExample extends App {
  import IntParser._

  val system: ActorSystem = ActorSystem("simpleAkka")
  val printer: ActorRef = system.actorOf(Printer.props, "printerActor")
  val hexParser: ActorRef = system.actorOf(IntParser.hexProps(printer), "hexParser")
  val decParser: ActorRef = system.actorOf(IntParser.decProps(printer), "decParser")

  hexParser ! Parse("FFFF")
  hexParser ! Print

  decParser ! Parse("1001")
  decParser ! Print

  hexParser ! Parse("F")
  hexParser ! Print

  decParser ! Parse("404")
  decParser ! Print

  hexParser ! Parse("404")
  hexParser ! Print
}
