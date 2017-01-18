package game

trait Direction
case object Left extends Direction
case object Right extends Direction
case object Up extends Direction
case object Down extends Direction

case class Event(seqNo: Long, command: Command)

trait Command
case class Movement(player: Player, direction: Direction) extends Command {
  def move(oldPos: Position, newPos: Position, board: Board): Board = {
    if(board.isValid(newPos)) {
      val newBoard = board.update(player.position, newPos, player)
      player.copy(position = newPos)
      return newBoard
    }
    board
  }
}

case class Join(player: Player) extends Command
case class Leave(name: String) extends Command

object CommandPatterns {
  val join = "join ([A-Za-z]+)".r
  val leave = "leave ([A-Za-z]+)".r
  val move = "move ([A-Za-z]+) (Left|Right|Up|Down)".r
  val print = "show"
  val fail = "boom"
}
object Utils {
  def getDirection: PartialFunction[String, Direction] = {
    case "Left" => Left
    case "Right" => Right
    case "Up" => Up
    case "Down" => Down
  }
  def createPlayer(name: String) = Player(name)
}
