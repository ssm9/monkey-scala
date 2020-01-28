package parsing

object Precedence extends Enumeration {
  val LOWEST = Value(0)
  val EQUALS = Value(1)
  val LESSGREATER = Value(2)
  val SUM = Value(3)
  val PRODUCT = Value(4)
  val PREFIX = Value(5)
  val CALL = Value(6)
}
