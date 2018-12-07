package hall

object Game {

  type FirstChoice = Int
  type OpenedDoor = Int

  val availablePositions: Set[FirstChoice] = Set(1, 2, 3)

  implicit class GetOpenDoor(set: Set[FirstChoice]) {
    def choose(): FirstChoice = set match {
      case x if x.size == 2 => x.toList(Field.getOpenedPositionIndex - 1)
      case x if x.size == 1 => x.head
    }
  }

  val totalAmount = 1000000

  val init: List[Field] = (1 to totalAmount).map(_ => Field()).toList

  val preChosen: List[(Field, FirstChoice, OpenedDoor)] = init.map { field =>
    val firstChoice = Field.getCarPosition
    val openedDoor = availablePositions.filterNot(x => x == firstChoice || x == field.answer).choose()
    (field, firstChoice, openedDoor)
  }

  val decisionChanged: List[(Field, Boolean)] = preChosen.map { field =>
    val finalDecision: FirstChoice = availablePositions.toList.filterNot(x => x == field._2 || x == field._3).head
    field._1 -> (finalDecision == field._1.answer)
  }

  val decisionNotChanged: List[(Field, Boolean)] = preChosen.map { field =>
    val finalDecision: FirstChoice = field._2
    field._1 -> (finalDecision == field._1.answer)
  }

  println(s"Decision changed: ${decisionChanged.count(_._2).toDouble/totalAmount.toDouble}")
  println(s"Decision not changed: ${decisionNotChanged.count(_._2).toDouble/totalAmount.toDouble}")

}
