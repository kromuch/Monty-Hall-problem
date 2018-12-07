import scala.annotation.switch

package object hall {

  sealed trait Door

  case class CarDoor() extends Door

  case class GoatDoor() extends Door

  trait Field {
    val door1: Door
    val door2: Door
    val door3: Door
    val answer: Int
  }

  object Field {
    val random = new scala.util.Random()

    def getCarPosition: Int = random.nextInt(3) + 1

    def getOpenedPositionIndex: Int = random.nextInt(2) + 1

    def apply(): Field = {
      val car = getCarPosition
      (car: @switch) match {
        case 1 => FieldImpl(CarDoor(), GoatDoor(), GoatDoor(), car)
        case 2 => FieldImpl(GoatDoor(), CarDoor(), GoatDoor(), car)
        case 3 => FieldImpl(GoatDoor(), GoatDoor(), CarDoor(), car)
      }
    }

    private case class FieldImpl(door1: Door, door2: Door, door3: Door, answer: Int) extends Field
  }


}
