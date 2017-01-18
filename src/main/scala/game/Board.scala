package game

case class Position(row: Int, col: Int) {
  def move: PartialFunction[Direction, Position] = {
    case Left => this.copy(col=col-1)
    case Right => this.copy(col=col+1)
    case Up => this.copy(row=row+1)
    case Down => this.copy(row=row-1)
  }
}
case class Player(name: String, position: Position = Position(0, 0))

case class Cell(players: List[Player] = List.empty){
  override def toString: String = {
    if(players.isEmpty) return "Empty"
    players.map(_.name.head).mkString("").padTo(5, ' ').toString
  }
  def addPlayer(player: Player) = this.copy(players = player :: players)
  def removePlayer(player: Player) = this.copy(players = players.filterNot(_.name == player.name))
}

case class Board(board: Array[Array[Cell]], maxRow: Int, maxCol: Int) {
  def isValid(pos: Position): Boolean = {
    -1 < pos.row && pos.row < maxRow && -1 < pos.col && pos.col < maxCol
  }
  def update(oldPos: Position, newPos: Position, player: Player): Board = {
    val oldRow = board(oldPos.row)
    val oldCell = oldRow(oldPos.col)
    val oldUpdatedRow = oldRow.updated(oldPos.col, oldCell.removePlayer(player))
    val newBoard = board.updated(oldPos.row, oldUpdatedRow)
    val newRow = newBoard(newPos.row)
    val newCell = newRow(newPos.col)
    val newUpdatedRow = newRow.updated(newPos.col, newCell.addPlayer(player))
    this.copy(board = newBoard.updated(newPos.row, newUpdatedRow))
  }
  override def toString = {
    board.map(_.map(cell => cell.toString).mkString("\t")).mkString("\n")
  }
  def addNewPlayer(player: Player) = {
    board(0)(0).addPlayer(player)
  }
}

