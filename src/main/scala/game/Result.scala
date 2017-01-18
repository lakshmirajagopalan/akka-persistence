package game

trait Result
case class PersistSuccess(command: Command) extends Result
case class PersistFailure(command: Command) extends Result
