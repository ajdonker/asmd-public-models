package u09.model

object QMatrix:

  type Node = (Int, Int)

  enum Move:
    case LEFT, RIGHT, UP, DOWN
    override def toString = Map(LEFT -> "<", RIGHT -> ">", UP -> "^", DOWN -> "v")(this)

  import Move.*

  case class Facade(
                     width: Int,
                     height: Int,
                     initial: Node,
                     terminal: PartialFunction[Node, Boolean],
                     reward: PartialFunction[(Node, Move), Double],
                     jumps: PartialFunction[(Node, Move), Node],
                     obstacles: Set[Node] = Set.empty,
                     obstaclePenalty: Double = -10.0,
                     gamma: Double,
                     alpha: Double,
                     epsilon: Double = 0.0,
                     v0: Double) extends QRLImpl:
    type State = Node
    type Action = Move

    private def normalDestination(state: Node, action: Move): Node =
      (state, action) match
        case ((x, y), UP) =>
          (x, (y - 1).max(0))

        case ((x, y), DOWN) =>
          (x, (y + 1).min(height - 1))

        case ((x, y), LEFT) =>
          ((x - 1).max(0), y)

        case ((x, y), RIGHT) =>
          ((x + 1).min(width - 1), y)

    def qEnvironment  (): Environment = (s: Node, a: Move) =>
        val ordinaryDestination = normalDestination(s,a)
        val destination = jumps.applyOrElse((s,a),(_: (Node, Move)) => ordinaryDestination)
        if obstacles.contains(destination) then
          (obstaclePenalty, s)
        else
          val transitionReward = reward.applyOrElse((s,a), (_: (Node, Move)) => 0.0)
          (transitionReward, destination)

    def qFunction = QFunction(Move.values.toSet, v0, terminal)
    def qSystem = QSystem(environment = qEnvironment(), initial, terminal)
    def makeLearningInstance() = QLearning(qSystem, gamma, alpha, epsilon, qFunction)

    def show[E](v: Node => E, formatString: String,
                obstacleString: String = "X"): String =
      (for
        row <- 0 until height
        col <- 0 until width
      yield
        val node = (col,row)
        val cell = {
            if obstacles.contains(node) then
              obstacleString
            else
              formatString.format(v(node))
        }
        cell + (if (col == width - 1) "\n" else "\t")
      )
        .mkString("")