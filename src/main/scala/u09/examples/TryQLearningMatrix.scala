package u09.examples

import u09.model.QMatrix

object TryQMatrix extends App :

  import u09.model.QMatrix.Move.*
  import u09.model.QMatrix.*
  val goal: Node = (8, 1)
  val corridorObstacles: Set[Node] = Set((2, 1),(2, 2),(4, 0),(4, 1), (6, 1), (6, 2))
  val rl: QMatrix.Facade = Facade(
    width = 9,
    height = 3,
    initial = (0,0),
    terminal = {case (8, 1) => true; case _ => false},
    reward = { case ((7,1), RIGHT) => 100.0; case _ => -0.1},
    jumps = PartialFunction.empty[(Node, Move), Node],
    //jumps = { case ((1,0),_) => (1,4); case ((3,0),_) => (3,2) },
    //obstacles = Set((2,2),(4,4)),
    obstacles = corridorObstacles,
    gamma = 0.95,
    alpha = 0.3,
    epsilon = 0.2,
    v0 = 0.0
  )

  val q0 = rl.qFunction
  println(rl.show(q0.vFunction,"%7.2f", "  X  "))
  val q1 = rl.makeLearningInstance().learn(30000,100,q0)
  println(rl.show(q1.vFunction,"%7.2f", "  X  "))
  println(rl.show(s => q1.bestPolicy(s).toString,"%7s", "  X  "))