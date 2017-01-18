package game

import akka.persistence._
import org.joda.time.DateTime
import CommandPatterns._
import Utils._

class GameState(length: Int) extends java.io.Serializable {
  var board = Board(Array.tabulate[Cell](length, length)((x, y) => Cell()), length, length)
  var players = List.empty[Player]

  def updateBoard(newBoard: Board): Unit ={
    board = newBoard
  }
  def addPlayerPos(player: Player, newPos: Position): Unit ={
    players = players.map({
      case p if p.name == player.name => p.copy(position = newPos)
      case p => p
    })
  }

  def addPlayer(player: Player): Unit = {
    players = player :: players
  }

  def removePlayer(name: String): Unit = {
    players = players.filterNot(_.name==name)
  }

  def getPlayer(name: String): Player = players.filter(_.name == name).head

  def show = board.toString + " \n " + players.toString()
}
class Game extends PersistentActor {
  var state = new GameState(10)
  var seqNo = DateTime.now.getMillis

  def getSeqNo = {
    seqNo = seqNo + 1
    seqNo - 1
  }

  def updateState: PartialFunction[Command, Unit] = {
    case movement: Movement =>
      val player = movement.player
      val newPos = player.position.move(movement.direction)
      val newBoard = movement.move(player.position, newPos ,state.board)

      state.updateBoard(newBoard)
      state.addPlayerPos(movement.player, newPos)

    case Join(player) => state.addPlayer(player)
    case Leave(name) => state.removePlayer(name)
  }

  override def receiveRecover: Receive = {
    case RecoveryCompleted => println("Recovery Finished")
    case event: Event => println(s"recovery - event $event");
      updateState(event.command)
    case SnapshotOffer(metadata, snapshot: GameState) => {
      state = snapshot
      println(s"reloaded metadata $metadata")
    }
  }

  override def receiveCommand: Receive = {
    case move(name, dir) =>
      val moveCommand = Movement(state.getPlayer(name), getDirection(dir))
      updateState(moveCommand)
      persist(Event(getSeqNo, moveCommand))(ev =>PersistSuccess(ev.command))
    case join(name) =>
      val joinCommand = Join(createPlayer(name))
      updateState(joinCommand)
      persist(Event(getSeqNo, joinCommand))(ev => PersistSuccess(ev.command))
    case leave(name) => //TODO: Need to update the board as well
      val leaveCommand = Leave(name)
      updateState(leaveCommand)
      persist(Event(getSeqNo, leaveCommand))(ev => PersistSuccess(ev.command))
    case "snap" => saveSnapshot(state)
    case SaveSnapshotSuccess(metadata) => println(s"SaveSnapshotSuccess(metadata): metadata=$metadata")
    case SaveSnapshotFailure(metadata, reason) => println(
      s"SaveSnapshotFailure(metadata, reason) : metadata=$metadata, reason=$reason")
    case show => println(state.show)
    case fail => throw new Exception("boom")
  }

  override def persistenceId: String = "game-persistent-actor-1"

  final case class SnapshotMetadata(persistenceId: String, sequenceNr: Long, timestamp: Long = 0L)
}
